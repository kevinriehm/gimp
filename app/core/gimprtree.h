/* GIMP - The GNU Image Manipulation Program
 * Copyright (C) 1995 Spencer Kimball and Peter Mattis
 *
 * gimprtree.h
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

#ifndef __GIMP_RTREE_H__
#define __GIMP_RTREE_H__


#include "gimpcontainer.h"


#define GIMP_TYPE_RTREE            (gimp_rtree_get_type ())
#define GIMP_RTREE(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GIMP_TYPE_RTREE, GimpRTree))
#define GIMP_RTREE_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GIMP_TYPE_RTREE, GimpRTreeClass))
#define GIMP_IS_RTREE(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GIMP_TYPE_RTREE))
#define GIMP_IS_RTREE_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GIMP_TYPE_RTREE))
#define GIMP_RTREE_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GIMP_TYPE_RTREE, GimpRTreeClass))


typedef struct _GimpRTreeClass GimpRTreeClass;
typedef struct _GimpRTreePriv  GimpRTreePriv;


struct _GimpRTree
{
  GimpContainer  parent_instance;

  GimpRTreePriv *priv;
};

struct _GimpRTreeClass
{
  GimpContainerClass  parent_class;

  gdouble           (*nearest_stroke_point_get) (const GimpRTree *rtree,
                                                 const GimpCoords *coord,
                                       const gdouble          precision,
                                                 GimpStroke **ret_stroke,
                                       GimpCoords            *ret_point,
                                       GimpAnchor           **ret_segment_start,
                                       GimpAnchor           **ret_segment_end,
                                       gdouble               *ret_pos);
};


GType           gimp_rtree_get_type             (void) G_GNUC_CONST;


#endif  /* __GIMP_RTREE_H__ */
