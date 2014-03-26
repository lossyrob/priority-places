package pps

import geotrellis._
import geotrellis.raster._
import geotrellis.source._

import geotrellis.raster.op.local.RasterReducer

object Model {
  def weightedOverlay(layers:Iterable[String], 
                      weights:Iterable[Int], 
                      rasterExtent:Option[RasterExtent]): RasterSource = {
    val weighted = 
      layers
        .zip(weights)
        .map { case (layer, weight) =>
          val rs =
            rasterExtent match {
              case Some(re) => RasterSource(layer, re)
              case None => RasterSource(layer)
            }
          rs.localMultiply(weight)
      }

    // Add will turn any computation involving NODATA to NODATA.
    // The data is set up so that some rasters have NODATA in cells that will
    // destroy the other layers values where it shouldn't. This version of Add
    // gets around this.
    weighted.toList match {
      case List() => sys.error("Cannot pass empty list of layers to weightedOverlay")
      case head :: List() => head
      case head :: tail => 
        head.combine(tail) { rasters =>
          new RasterReducer({ 
            (z1: Int, z2: Int) => if(isNoData(z1)) z2 else if (isNoData(z2)) z1 else z1 + z2 
          })({
            (z1: Double, z2: Double) => if(isNoData(z1)) z2 else if (isNoData(z2)) z1 else z1 + z2 
          })(rasters)
        }
    }
  }
}
