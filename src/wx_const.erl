-module(wx_const).
-compile(export_all).

-include_lib("wx/include/wx.hrl").

wx_horizontal() ->
    ?wxHORIZONTAL.

wx_vertical() ->
    ?wxVERTICAL.

wx_expand() ->
    ?wxEXPAND.

wx_all() ->
    ?wxALL.

wx_black_pen() ->
    ?wxBLACK_PEN.

wx_full_repaint_on_resize() ->
    ?wxFULL_REPAINT_ON_RESIZE.

wx_italic_font() ->
    ?wxITALIC_FONT.

wx_id_any() ->
    ?wxID_ANY.

wx_sunken_border() ->
    ?wxSUNKEN_BORDER.

wx_transparent_brush() ->
    ?wxTRANSPARENT_BRUSH.

wx_transparent_pen() ->
    ?wxTRANSPARENT_PEN.

wx_black() ->
    ?wxBLACK.

wx_red_brush() ->
    ?wxRED_BRUSH.

wx_blue_brush() ->
    ?wxBLUE_BRUSH.

wx_green_brush() ->
    ?wxGREEN_BRUSH.

wx_light_grey_pen() ->
    ?wxLIGHT_GREY_PEN.

wx_red_pen() ->
    ?wxRED_PEN.
