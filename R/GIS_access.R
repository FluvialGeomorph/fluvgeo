# GIS Access


arc2sp <- function(fc_path) {
  # Opens an ArcGIS feature class and converts it to sp format.
  #
  # Args:
  #    fc_path:          string; Path to the ArcGIS feature class.
  #
  # Returns:
  #    the ArcGIS feature class specified by path as an sp object
  #
  arcobj <- arc.open(fc_path)
  arc <- arc.select(arcobj)
  sp <- arc.data2sp(arc)
  return(sp)
}

sp2arc <- function(sp_obj, fc_path) {
  # Saves an sp object to a geodatabase.
  #
  # Args:
  #    sp_object         sp object;
  #    fc_path:          string; Path to the ArcGIS feature class.
  #
  # Returns:
  #    writes the sp object to an ArcGIS feature class specified by path
  #
  arcobj <- arc.sp2data(sp_obj)
  arc.write(arcobj, fc_path)
}
