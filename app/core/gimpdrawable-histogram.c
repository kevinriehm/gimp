/* GIMP - The GNU Image Manipulation Program
 * Copyright (C) 1995 Spencer Kimball and Peter Mattis
 *
 * gimphistogram module Copyright (C) 1999 Jay Cox <jaycox@gimp.org>
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

#include <gdk-pixbuf/gdk-pixbuf.h>
#include <gegl.h>

#include "core-types.h"

#include "gegl/gimp-gegl-nodes.h"

#include "gimpchannel.h"
#include "gimpdrawable-histogram.h"
#include "gimpdrawableundo.h"
#include "gimphistogram.h"
#include "gimpimage.h"
#include "gimpimage-undo.h"
#include "gimpundostack.h"


void
gimp_drawable_calculate_histogram (GimpDrawable  *drawable,
                                   GimpHistogram *histogram)
{
  GimpImage        *image;
  GimpChannel      *mask;
  GimpUndo         *undo;
  GeglBuffer       *old_buffer;
  GimpUndoStack    *redo_stack, *undo_stack, *undo_group;
  gint              x, y, width, height;
  gint              old_x, old_y, old_width, old_height;

  g_return_if_fail (GIMP_IS_DRAWABLE (drawable));
  g_return_if_fail (gimp_item_is_attached (GIMP_ITEM (drawable)));
  g_return_if_fail (histogram != NULL);

  if (! gimp_item_mask_intersect (GIMP_ITEM (drawable), &x, &y, &width, &height))
    return;

  image = gimp_item_get_image (GIMP_ITEM (drawable));
  mask  = gimp_image_get_mask (image);

  /* Check the undo stack. */
  old_buffer = NULL;
  redo_stack = gimp_image_get_redo_stack (image);
  if (!gimp_undo_stack_peek(redo_stack))
    {
      undo_stack = gimp_image_get_undo_stack (image);
      undo = gimp_undo_stack_peek (undo_stack);
      if (undo && undo->undo_type == GIMP_UNDO_GROUP_PAINT)
        {
          undo = gimp_undo_stack_peek (GIMP_UNDO_STACK (undo));
          if (undo && undo->undo_type == GIMP_UNDO_DRAWABLE)
            {
              g_object_get (GIMP_DRAWABLE_UNDO (undo),
                            "buffer", &old_buffer,
                            "x", &old_x,
                            "y", &old_y,
                            NULL);
              old_width = gegl_buffer_get_width (old_buffer);
              old_height = gegl_buffer_get_height (old_buffer);

              if (x < old_x)
                {
                  width -= old_x - x;
                  x = old_x;
                }
              else
                {
                  old_width -= x - old_x;
                  old_x = x;
                }

              if (y < old_y)
                {
                  height -= old_y - y;
                  y = old_y;
                }
              else
                {
                  old_height -= y - old_y;
                  old_y = y;
                }

              if (old_width < width)
                  width = old_width;

              if (old_height < height)
                  height = old_height;
            }
        }
    }

  if (FALSE)
    {
      GeglNode      *node = gegl_node_new ();
      GeglNode      *buffer_source;
      GeglNode      *histogram_sink;
      GeglProcessor *processor;

      buffer_source =
        gimp_gegl_add_buffer_source (node,
                                     gimp_drawable_get_buffer (drawable),
                                     0, 0);

      histogram_sink =
        gegl_node_new_child (node,
                             "operation", "gimp:histogram-sink",
                             "histogram", histogram,
                             NULL);

      gegl_node_connect_to (buffer_source,  "output",
                            histogram_sink, "input");

      if (! gimp_channel_is_empty (mask))
        {
          GeglNode *mask_source;
          gint      off_x, off_y;

          g_printerr ("adding mask aux\n");

          gimp_item_get_offset (GIMP_ITEM (drawable), &off_x, &off_y);

          mask_source =
            gimp_gegl_add_buffer_source (node,
                                         gimp_drawable_get_buffer (GIMP_DRAWABLE (mask)),
                                         -off_x, -off_y);

          gegl_node_connect_to (mask_source,    "output",
                                histogram_sink, "aux");
        }

      processor = gegl_node_new_processor (histogram_sink,
                                           GEGL_RECTANGLE (x, y, width, height));

      while (gegl_processor_work (processor, NULL));

      g_object_unref (processor);
      g_object_unref (node);
    }
  else
    {
int count = 0;
struct timeval begin;
gettimeofday(&begin,NULL);
      if (! gimp_channel_is_empty (mask))
        {
          gint off_x, off_y;

          gimp_item_get_offset (GIMP_ITEM (drawable), &off_x, &off_y);

          gimp_histogram_calculate (histogram,
                                    old_buffer,
                                    gimp_drawable_get_buffer (drawable),
                                    GEGL_RECTANGLE (x, y, width, height),
                                    gimp_drawable_get_buffer (GIMP_DRAWABLE (mask)),
                                    GEGL_RECTANGLE (x + off_x, y + off_y,
                                                    width, height));
        }
      else
        {
          gimp_histogram_calculate (histogram,
                                    old_buffer,
                                    gimp_drawable_get_buffer (drawable),
                                    GEGL_RECTANGLE (x, y, width, height),
                                    NULL, NULL);
        }
struct timeval end;
gettimeofday(&end,NULL);
fprintf(stderr,"%.3fms : %i : lalala\n",(end.tv_sec*1000. + end.tv_usec/1000.) - (begin.tv_sec*1000. + begin.tv_usec/1000.),count);
    }

    if (old_buffer)
      g_object_unref (old_buffer);
}
