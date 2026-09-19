\name{survey_collection_products}
\alias{survey_collection_products}
\title{Review product evidence for discovered Survey Collections}
\usage{survey_collection_products(discovery)}
\arguments{\item{discovery}{Output of discover_survey_collections() or its saved snapshot.}}
\value{Nonspatial table with two product rows per catalog record. Links are reported, not tested. General catalog access is not product-specific evidence.
dem_pixel_size_m uses explicit USGS dem_gsd_meters only; missing/nonpositive
values are unknown. A pixel size at most 1 m passes resolution screening only,
not scientific suitability. Point spacing is never treated as raster size.}
\description{Review source DEM and point-cloud evidence without network requests or inferred availability.}
