
stream <- "Cole Creek R1"
flowline_fc <- "D:\\Workspace\\EMRRP_Sediment\\PapillionCreek_NE\\Reaches\\02_Cole_Creek\\y2016_R1.gdb\\flowline"
cross_section_fc <- "D:\\Workspace\\EMRRP_Sediment\\PapillionCreek_NE\\Reaches\\02_Cole_Creek\\y2016_R1.gdb\\xs_50"
flowline_points_1 <- "D:\\Workspace\\EMRRP_Sediment\\PapillionCreek_NE\\Reaches\\02_Cole_Creek\\y2004_R1.gdb\\flowline_points"
flowline_points_2 <- "D:\\Workspace\\EMRRP_Sediment\\PapillionCreek_NE\\Reaches\\02_Cole_Creek\\y2010_R1.gdb\\flowline_points"
flowline_points_3 <- "D:\\Workspace\\EMRRP_Sediment\\PapillionCreek_NE\\Reaches\\02_Cole_Creek\\y2016_R1.gdb\\flowline_points"
flowline_points_4 <- NULL
xs_points_1 <- "D:\\Workspace\\EMRRP_Sediment\\PapillionCreek_NE\\Reaches\\02_Cole_Creek\\y2004_R1.gdb\\xs_50_points"
xs_points_2 <- "D:\\Workspace\\EMRRP_Sediment\\PapillionCreek_NE\\Reaches\\02_Cole_Creek\\y2010_R1.gdb\\xs_50_points"
xs_points_3 <- "D:\\Workspace\\EMRRP_Sediment\\PapillionCreek_NE\\Reaches\\02_Cole_Creek\\y2016_R1.gdb\\xs_50_points"
xs_points_4 <- NULL
survey_name_1 <- "2004"
survey_name_2 <- "2010"
survey_name_3 <- "2016"
survey_name_4 <- NULL
features_fc <- "D:\\Workspace\\EMRRP_Sediment\\PapillionCreek_NE\\Reaches\\02_Cole_Creek\\y2016_R1.gdb\\features"
profile_units <- "feet"
output_dir <- "D:/Workspace/EMRRP_Sediment/PapillionCreek_NE/Reaches/02_Cole_Creek"
output_format <- "word_document"


fluvgeo::level_1_report(stream, flowline_fc, cross_section_fc,
                        flowline_points_1, flowline_points_2,
                        flowline_points_3, flowline_points_4,
                        xs_points_1, xs_points_2, xs_points_3, xs_points_4,
                        survey_name_1, survey_name_2,
                        survey_name_3, survey_name_4,
                        features_fc, profile_units,
                        output_dir, output_format)
