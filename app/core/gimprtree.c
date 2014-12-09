/* GIMP - The GNU Image Manipulation Program
 * Copyright (C) 1995-1997 Spencer Kimball and Peter Mattis
 *
 * gimpcontainer.c
 * Copyright (C) 2001 Michael Natterer <mitch@gimp.org>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "config.h"

#include <math.h>
#include <stdio.h>

#include <gio/gio.h>

#include "core-types.h"

#include "vectors/vectors-types.h"
#include "vectors/gimpstroke.h"

#include "gimprtree.h"


#define NODE_MAX_CHILDREN 500
#define NODE_MIN_CHILDREN 2


enum
{
  PROP_0,
  PROP_BOUNDS_FUNC
};


typedef struct _GimpRTreeNode  GimpRTreeNode;
typedef struct _GimpRTreeChild GimpRTreeChild;

struct _GimpRTreeChild {
    gdouble       x1, x2;
    gdouble       y1, y2;

    union {
      GimpRTreeNode *node;
      GimpObject    *object;
    } data;
};

struct _GimpRTreeNode {
  GimpRTreeNode *parent;

  gint           n_children;
  gboolean       has_leaves; /* Whether children are leaves. */

  GimpRTreeChild children[NODE_MAX_CHILDREN];
};

struct _GimpRTreePriv
{
  GimpBoundsFunc bounds_func;

  GimpRTreeNode *root;
};

/*  local function prototypes  */

static void         gimp_rtree_finalize           (GObject             *object);

static void         gimp_rtree_set_property       (GObject             *object,
                                                   guint                property_id,
                                                   const GValue        *value,
                                                   GParamSpec          *pspec);
static void         gimp_rtree_get_property       (GObject             *object,
                                                   guint                property_id,
                                                   GValue              *value,
                                                   GParamSpec          *pspec);

static void         gimp_rtree_add                (GimpContainer       *container,
                                                   GimpObject          *object);
static void         gimp_rtree_remove             (GimpContainer       *container,
                                                   GimpObject          *object);
static void         gimp_rtree_reorder            (GimpContainer       *container,
                                                   GimpObject          *object,
                                                   gint                 new_index);
static void         gimp_rtree_clear              (GimpContainer       *container);
static gboolean     gimp_rtree_have               (const GimpContainer *container,
                                                   const GimpObject    *object);
static void         gimp_rtree_foreach            (const GimpContainer *container,
                                                   GFunc                func,
                                                   gpointer             user_data);
static GimpObject * gimp_rtree_get_child_by_name  (const GimpContainer *container,
                                                   const gchar         *name);
static GimpObject * gimp_rtree_get_child_by_index (const GimpContainer *container,
                                                   gint                 index);
static gint         gimp_rtree_get_child_index    (const GimpContainer *container,
                                                   const GimpObject    *object);

static 
  gdouble           gimp_rtree_nearest_stroke_point_get (const GimpRTree *rtree,
                                       const GimpCoords *coord,
                                       const gdouble          precision,
                                       GimpStroke **ret_stroke,
                                       GimpCoords            *ret_point,
                                       GimpAnchor           **ret_segment_start,
                                       GimpAnchor           **ret_segment_end,
                                       gdouble               *ret_pos);


G_DEFINE_TYPE (GimpRTree, gimp_rtree, GIMP_TYPE_CONTAINER)

#define parent_class gimp_rtree_parent_class


static void
gimp_rtree_class_init (GimpRTreeClass *klass)
{
  GObjectClass       *object_class      = G_OBJECT_CLASS (klass);
  GimpObjectClass    *gimp_object_class = GIMP_OBJECT_CLASS (klass);
  GimpContainerClass *container_class   = GIMP_CONTAINER_CLASS (klass);

  object_class->finalize              = gimp_rtree_finalize;
  object_class->set_property          = gimp_rtree_set_property;
  object_class->get_property          = gimp_rtree_get_property;

  container_class->add                = gimp_rtree_add;
  container_class->remove             = gimp_rtree_remove;
  container_class->reorder            = gimp_rtree_reorder;
  container_class->clear              = gimp_rtree_clear;
  container_class->have               = gimp_rtree_have;
  container_class->foreach            = gimp_rtree_foreach;
  container_class->get_child_by_name  = gimp_rtree_get_child_by_name;
  container_class->get_child_by_index = gimp_rtree_get_child_by_index;
  container_class->get_child_index    = gimp_rtree_get_child_index;

  klass->nearest_stroke_point_get = gimp_rtree_nearest_stroke_point_get;

  g_object_class_install_property (object_class, PROP_BOUNDS_FUNC,
                                   g_param_spec_pointer ("bounds-func",
                                                         NULL, NULL,
                                                         GIMP_PARAM_READWRITE |
                                                         G_PARAM_CONSTRUCT_ONLY));

  g_type_class_add_private (object_class, sizeof (GimpRTreePriv));
}


static void
gimp_rtree_init (GimpRTree *rtree)
{
  rtree->priv = G_TYPE_INSTANCE_GET_PRIVATE (rtree,
                                             GIMP_TYPE_RTREE,
                                             GimpRTreePriv);
  rtree->priv->bounds_func = NULL;
  rtree->priv->root = g_new(GimpRTreeNode,1);
  rtree->priv->root->parent = NULL;
  rtree->priv->root->n_children = 0;
  rtree->priv->root->has_leaves = TRUE;
}


static void
gimp_rtree_finalize (GObject *object)
{
  gimp_rtree_clear (GIMP_CONTAINER (object));

  g_free(GIMP_RTREE (object)->priv->root);

  G_OBJECT_CLASS (parent_class)->finalize (object);
}


static void
gimp_rtree_set_property (GObject      *object,
                         guint         property_id,
                         const GValue *value,
                         GParamSpec   *pspec)
{
  GimpRTree *rtree = GIMP_RTREE (object);

  switch (property_id)
    {
    case PROP_BOUNDS_FUNC:
      rtree->priv->bounds_func = g_value_get_pointer (value);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
    }
}


static void
gimp_rtree_get_property (GObject    *object,
                         guint       property_id,
                         GValue     *value,
                         GParamSpec *pspec)
{
  GimpRTree *rtree = GIMP_RTREE (object);

  switch (property_id)
    {
    case PROP_BOUNDS_FUNC:
      g_value_set_pointer (value, rtree->priv->bounds_func);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
    }
}


static void
gimp_rtree_update_bounds (GimpRTreeNode *node)
{
  while (node->parent)
    {
      gint index;

      for (index = 0; index < node->parent->n_children; index++)
        if (node->parent->children[index].data.node == node)
          break;

      gdouble *x1 = &node->parent->children[index].x1,
              *x2 = &node->parent->children[index].x2,
              *y1 = &node->parent->children[index].y1,
              *y2 = &node->parent->children[index].y2;

      *x1 =  G_MAXDOUBLE;
      *x2 = -G_MAXDOUBLE;
      *y1 =  G_MAXDOUBLE;
      *y2 = -G_MAXDOUBLE;

      for (index = 0; index < node->n_children; index++)
        {
          *x1 = MIN (*x1, node->children[index].x1);
          *x2 = MAX (*x2, node->children[index].x2);
          *y1 = MIN (*y1, node->children[index].y1);
          *y2 = MAX (*y2, node->children[index].y2);
        }

      node = node->parent;
   }
}

/* ChooseSubtree  */
static GimpRTreeNode *
gimp_rtree_choose_subtree (GimpRTreeNode  *root,
                           GimpRTreeChild *new_child)
{fprintf(stderr,"gimp_rtree_choose_subtree()\n");
  gint i;

  while (!root->has_leaves)
    {
      gint    min_child;
      gdouble min_area     = G_MAXDOUBLE;
      gdouble min_increase = G_MAXDOUBLE;

      gdouble child_x1, child_x2;
      gdouble child_y1, child_y2;

      /* Find the child whose area will increase the least by inserting object. */
      for (i = 0; i < root->n_children; i++)
        {
          child_x1 = MIN (root->children[i].x1, new_child->x1);
          child_x2 = MAX (root->children[i].x2, new_child->x2);
          child_y1 = MIN (root->children[i].y1, new_child->y1);
          child_y2 = MAX (root->children[i].y2, new_child->y2);

          gdouble area = (root->children[i].x2 - root->children[i].x1)*(root->children[i].y2 - root->children[i].y1);
          gdouble increase = (child_x2 - child_x1)*(child_y2 - child_y1) - area;

          if (increase < min_increase || increase == min_increase && area < min_area)
            {
              min_child    = i;
              min_area     = area;
              min_increase = increase;
            }
        }

      root->children[i].x1 = child_x1;
      root->children[i].x2 = child_x2;
      root->children[i].y1 = child_y1;
      root->children[i].y2 = child_y2;

      root = root->children[min_child].data.node;
    }

  return root;
}

static void
gimp_rtree_pick_seeds (GimpRTreeNode  *node,
                       GimpRTreeChild *new_child,
                       gboolean        sorted[static NODE_MAX_CHILDREN + 1],
                       GimpRTreeChild *group1,
                       GimpRTreeChild *group2)
{
  gint            i, j;
  gdouble         max_d = -G_MAXDOUBLE;
  GimpRTreeChild *max_seed1, *max_seed2;
  gint            max_seed1i, max_seed2i;

  for (i = 0; i < NODE_MAX_CHILDREN; i++)
    {
      for (j = i + 1; j < NODE_MAX_CHILDREN + 1; j++)
        {
          GimpRTreeChild *child1 = node->children + i,
                         *child2 = j == NODE_MAX_CHILDREN ? new_child : node->children + j;

          gdouble d = (MAX (child1->x2, child2->x2) - MIN (child1->x1, child2->x1))*(MAX (child1->y2, child2->y2) - MIN (child1->y1, child2->y1))
            - (child1->x2 - child1->x1)*(child1->y2 - child1->y1) - (child2->x2 - child2->x1)*(child2->y2 - child2->y1);

          if (d > max_d)
            {
              max_d = d;
              max_seed1 = child1;
              max_seed2 = child2;
              max_seed1i = i;
              max_seed2i = j;
            }
        }
    }

  if (!group1->data.node->has_leaves)
    max_seed1->data.node->parent = group1->data.node;
  if (!group2->data.node->has_leaves)
    max_seed2->data.node->parent = group2->data.node;

  group1->data.node->children[group1->data.node->n_children++] = *max_seed1;
  group2->data.node->children[group2->data.node->n_children++] = *max_seed2;

  sorted[max_seed1i] = TRUE;
  sorted[max_seed2i] = TRUE;
}

/* PickNext */
static void
gimp_rtree_pick_next (GimpRTreeChild  children[static NODE_MAX_CHILDREN],
                      GimpRTreeChild *new_child,
                      gboolean        sorted[static NODE_MAX_CHILDREN + 1],
                      GimpRTreeChild *group1,
                      GimpRTreeChild *group2)
{
  gint            i;
  gdouble         max_d = -G_MAXDOUBLE;
  gdouble         max_dd1, max_dd2;
  gint            max_di;
  GimpRTreeChild *max_dchild;

  /* Minimize the area increase from putting child i into group 1 or group 2. */
  for (i = 0; i < NODE_MAX_CHILDREN + 1; i++)
    {
      if (sorted[i])
        continue;

      GimpRTreeChild *child = i == NODE_MAX_CHILDREN ? new_child : children + i;

      gdouble d1 = (MAX (child->x2, group1->x2) - MIN (child->x1, group1->x1))*(MAX (child->y2, group1->y2) - MIN (child->y1, group1->y1)),
              d2 = (MAX (child->x2, group2->x2) - MIN (child->x1, group2->x1))*(MAX (child->y2, group2->y2) - MIN (child->y1, group2->y1));

      if (ABS (d1 - d2) > max_d)
        {
          max_d  = ABS (d1 - d2);
          max_dd1 = d1;
          max_dd2 = d2;
          max_di = i;
          max_dchild = child;
        }
    }

  sorted[max_di] = TRUE;

  GimpRTreeNode *group_node = (max_dd1 < max_dd2 ? group1 : group2)->data.node;
  if (group_node->n_children >= NODE_MAX_CHILDREN)
    group_node = (max_dd1 < max_dd2 ? group2 : group1)->data.node;

  group_node->children[group_node->n_children++] = *max_dchild;
  if (!group_node->has_leaves)
    max_dchild->data.node->parent = group_node;
}

/* QuadraticSplit */
static void
gimp_rtree_insert_into_node (GimpRTreeNode  *node,
                             GimpRTreeChild *new_child)
{
  gint i;

  /* Simple case: already enough room in node. */
  if (node->n_children < NODE_MAX_CHILDREN)
    {
      node->children[node->n_children++] = *new_child;

      if (!node->has_leaves)
        {
          new_child->data.node->parent = node;
          gimp_rtree_update_bounds (new_child->data.node);
        }

      gimp_rtree_update_bounds (node);
      return;
    }

  /* Complex case: quadratic split of node.    */
  GimpRTreeChild group1;
  group1.data.node = g_new (GimpRTreeNode, 1);
  group1.data.node->parent = NULL;
  group1.data.node->n_children = 0;
  group1.data.node->has_leaves = node->has_leaves;

  GimpRTreeChild group2;
  group2.data.node = g_new (GimpRTreeNode, 1);
  group2.data.node->parent = NULL;
  group2.data.node->n_children = 0;
  group2.data.node->has_leaves = node->has_leaves;

  gboolean sorted[NODE_MAX_CHILDREN + 1] = {0};

  gimp_rtree_pick_seeds (node, new_child, sorted, &group1, &group2);

  for (i = 2; i < NODE_MAX_CHILDREN + 1; i++)
    gimp_rtree_pick_next (node->children, new_child, sorted, &group1, &group2);

  if (node->parent)
    {
      for (i = 0; i < node->parent->n_children; i++)
        {
          if (node->parent->children[i].data.node == node)
            {
              group1.data.node->parent = node->parent;
              node->parent->children[i] = group1;
              break;
            }
        }

      gimp_rtree_update_bounds (group1.data.node);

      gimp_rtree_insert_into_node (node->parent, &group2);

      g_free (node);
    }
  else /* node is the root. */
    {
      node->n_children = 2;
      node->has_leaves = FALSE;

      group1.data.node->parent = node;
      group2.data.node->parent = node;

      node->children[0] = group1;
      node->children[1] = group2;

      gimp_rtree_update_bounds (group1.data.node);
      gimp_rtree_update_bounds (group2.data.node);
    }
}
static void print_rtree(GimpRTreeNode *node, int level) {
	int i;
	char prefix[1000];
	memset(prefix,'-',level*8);
	prefix[level*8] = '\0';
	fprintf(stderr,"\n\n\n%s[%p]\n",prefix,node);
	fprintf(stderr,"%sparent = [%p]\n",prefix,node->parent);
	fprintf(stderr,"%shas_leaves = %i\n",prefix,node->has_leaves);
	fprintf(stderr,"%sn_children = %i\n",prefix,node->n_children);
	for(i = 0; i < node->n_children; i++) {
		fprintf(stderr,"%schild %i: <%f, %f> x <%f, %f>\n",prefix,i,node->children[i].x1,node->children[i].y1,node->children[i].x2,node->children[i].y2);
		if(node->has_leaves)
			fprintf(stderr,"%s|[%p]\n",prefix,node->children[i].data.object);
		else print_rtree(node->children[i].data.node,level + 1);
	}
}

static void
gimp_rtree_add (GimpContainer *container,
                GimpObject    *object)
{fprintf(stderr,"gimp_rtree_add(%p, %p)\n",container,object);
  g_return_if_fail (GIMP_IS_RTREE (container));

  GimpRTree *rtree = GIMP_RTREE (container);

  GimpRTreeNode *node;
  GimpRTreeChild child;

  rtree->priv->bounds_func (object, &child.x1, &child.x2, &child.y1, &child.y2);

  child.data.object = object;

  node = gimp_rtree_choose_subtree (rtree->priv->root, &child);
  gimp_rtree_insert_into_node (node, &child);

  GIMP_CONTAINER_CLASS (parent_class)->add (container, object);
print_rtree(rtree->priv->root,0);
}


static gboolean
gimp_rtree_find_entry (GimpRTreeNode  *node,
                       GimpRTreeChild *child,
                       GimpRTreeNode **ret_node,
                       gint           *ret_index)
{
  gint i;

  for (i = 0; i < node->n_children; i++)
    {
      if (node->has_leaves)
        {
          if (node->children[i].data.object == child->data.object)
            {
              if (ret_node)
                *ret_node = node;
              if (ret_index)
                *ret_index = i;
              return TRUE;
            }
        }
      else
        {
          if (node->children[i].x1 <= child->x1 && child->x2 <= node->children[i].x2
              && node->children[i].y1 <= child->y1 && child->y2 <= node->children[i].y2
              && gimp_rtree_find_entry (node->children[i].data.node, child, ret_node, ret_index))
            return TRUE;
        }
    }

  return FALSE;
}

static void
gimp_rtree_remove (GimpContainer *container,
                   GimpObject    *object)
{
  GimpRTree *rtree = GIMP_RTREE (container);

  gint index;
  GimpRTreeNode *node;
  GimpRTreeChild child;

  rtree->priv->bounds_func (object, &child.x1, &child.x2, &child.y1, &child.y2);
  child.data.object = object;

  if (gimp_rtree_find_entry (GIMP_RTREE (container)->priv->root, &child, &node, &index))
    {
      do {
        for (; index < node->n_children - 1; index++)
          node->children[index] = node->children[index + 1];

        index = -1;

        node->n_children--;
        if (node->n_children == 0 && node->parent)
          {
            for (index = 0; index < node->parent->n_children; index++)
              if (node->children[index].data.node == node)
                break;
            node = node->parent;
          }
      } while (index >= 0);

      gimp_rtree_update_bounds (node);
    }

  GIMP_CONTAINER_CLASS (parent_class)->remove (container, object);
}


static void
gimp_rtree_reorder (GimpContainer *container,
                    GimpObject    *object,
                    gint           new_index)
{
  g_printerr ("gimp_rtree_reorder: not implemented\n");
}


static void
gimp_rtree_clear_node (GimpRTreeNode *node)
{
  gint i;

  if (!node->has_leaves)
    for (i = 0; i < node->n_children; i++)
      gimp_rtree_clear_node (node->children[i].data.node);

  g_free (node);
}

static void
gimp_rtree_clear (GimpContainer *container)
{
  g_return_if_fail (GIMP_IS_RTREE (container));

  GimpRTree *rtree = GIMP_RTREE (container);

  gimp_rtree_clear_node (rtree->priv->root);
  rtree->priv->root = NULL;
}


static gboolean
gimp_rtree_have (const GimpContainer *container,
                 const GimpObject    *object)
{
  g_return_val_if_fail (GIMP_IS_RTREE (container), FALSE);

  GimpRTree *rtree = GIMP_RTREE (container);

  GimpRTreeChild child;
  rtree->priv->bounds_func (object, &child.x1, &child.x2, &child.y1, &child.y2);
  child.data.object = GIMP_OBJECT (object);

  return gimp_rtree_find_entry (rtree->priv->root, &child, NULL, NULL);
}


static void
gimp_rtree_foreach (const GimpContainer *container,
                    GFunc                func,
                    gpointer             user_data)
{
  g_printerr ("gimp_rtree_foreach: not implemented\n");
}


static GimpObject *
gimp_rtree_get_child_by_name (const GimpContainer *container,
                              const gchar         *name)
{
  g_printerr ("gimp_rtree_get_child_by_name: not implemented\n");
}


static GimpObject *
gimp_rtree_get_child_by_index (const GimpContainer *container,
                               gint                 index)
{
  g_printerr ("gimp_rtree_get_child_by_index: not implemented\n");
}


static gint
gimp_rtree_get_child_index (const GimpContainer *container,
                            const GimpObject    *object)
{
  g_printerr ("gimp_rtree_get_child_index: not implemented\n");
}


GimpContainer *
gimp_rtree_new (GType children_type)
{
  GimpRTree *rtree;

  g_return_val_if_fail (g_type_is_a (children_type, GIMP_TYPE_OBJECT), NULL);

  rtree = g_object_new (GIMP_TYPE_RTREE,
                        "children-type",children_type,
                        "policy",GIMP_CONTAINER_POLICY_WEAK,
                        NULL);

  return GIMP_CONTAINER (rtree);
}

static gdouble
gimp_rtree_node_nearest_stroke_point_get (const GimpRTreeNode   *node,
                                          const GimpCoords      *coord,
                                          const gdouble          precision,
                                          GimpStroke           **ret_stroke,
                                          GimpCoords            *ret_coords,
                                          GimpAnchor           **ret_segment_start,
                                          GimpAnchor           **ret_segment_end,
                                          gdouble               *ret_pos)
{
  gint i;
  gdouble x_dist, y_dist, dist;
  gdouble min_dist = G_MAXDOUBLE;

  GimpStroke *cur_stroke;
  GimpCoords  cur_coords;
  GimpAnchor *cur_segment_start;
  GimpAnchor *cur_segment_end;
  gdouble     cur_dist, cur_pos;

  for (i = 0; i < node->n_children; i++)
    {
      /* Closest point in the child. */
      x_dist = coord->x < node->children[i].x1 ? node->children[i].x1 - coord->x
        : coord->x < node->children[i].x2 ? 0
        : coord->x - node->children[i].x2;
      y_dist = coord->y < node->children[i].y1 ? node->children[i].y1 - coord->y
        : coord->y < node->children[i].y2 ? 0
        : coord->y - node->children[i].y2;
      dist = sqrt (x_dist*x_dist + y_dist*y_dist);
fprintf(stderr,"dist = %f, min_dist = %f\n",dist,min_dist);
      if (dist < min_dist)
        {
          if (node->has_leaves) {
              cur_stroke = GIMP_STROKE (node->children[i].data.object);fprintf(stderr,"cur_stroke = %p\n",cur_stroke);
              cur_dist = gimp_stroke_nearest_point_get (cur_stroke, coord,
                                             precision, &cur_coords, &cur_segment_start, &cur_segment_end, &cur_pos);
          } else cur_dist = gimp_rtree_node_nearest_stroke_point_get (node->children[i].data.node, coord,
                                             precision, &cur_stroke, &cur_coords, &cur_segment_start, &cur_segment_end, &cur_pos);
fprintf(stderr,"cur_dist = %f\n",cur_dist);
          if (cur_dist >= 0 && cur_dist < min_dist)
            {
              min_dist = cur_dist;

              if (ret_stroke)        *ret_stroke        = cur_stroke;
              if (ret_coords)        *ret_coords        = cur_coords;
              if (ret_pos)           *ret_pos           = cur_pos;
              if (ret_segment_start) *ret_segment_start = cur_segment_start;
              if (ret_segment_end)   *ret_segment_end   = cur_segment_end;
            }
        }
    }

  return min_dist;
}

static gdouble
gimp_rtree_nearest_stroke_point_get (const GimpRTree *rtree,
                                     const GimpCoords *coord,
                                     const gdouble          precision,
                                     GimpStroke           **ret_stroke,
                                     GimpCoords            *ret_point,
                                     GimpAnchor           **ret_segment_start,
                                     GimpAnchor           **ret_segment_end,
                                     gdouble               *ret_pos)
{
  return gimp_rtree_node_nearest_stroke_point_get (rtree->priv->root, coord, precision, ret_stroke, ret_point, ret_segment_start, ret_segment_end, ret_pos);
}
