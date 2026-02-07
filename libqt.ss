(export #t)
(import :std/foreign)

(begin-ffi
    (;; Alignment constants
     QT_ALIGN_LEFT QT_ALIGN_RIGHT QT_ALIGN_CENTER
     QT_ALIGN_TOP QT_ALIGN_BOTTOM

     ;; Echo mode constants
     QT_ECHO_NORMAL QT_ECHO_NO_ECHO QT_ECHO_PASSWORD QT_ECHO_PASSWORD_ON_EDIT

     ;; Application lifecycle
     qt_application_create qt_application_exec qt_application_quit
     qt_application_process_events qt_application_destroy

     ;; Widget base
     qt_widget_create qt_widget_show qt_widget_hide qt_widget_close
     qt_widget_set_enabled qt_widget_is_enabled qt_widget_set_visible
     qt_widget_set_fixed_size qt_widget_set_minimum_size
     qt_widget_set_maximum_size qt_widget_resize
     qt_widget_set_style_sheet qt_widget_set_tooltip
     qt_widget_set_font_size qt_widget_destroy

     ;; Main Window
     qt_main_window_create qt_main_window_set_title
     qt_main_window_set_central_widget

     ;; Layouts
     qt_vbox_layout_create qt_hbox_layout_create
     qt_layout_add_widget qt_layout_add_stretch
     qt_layout_set_spacing qt_layout_set_margins

     ;; Labels
     qt_label_create qt_label_set_text qt_label_text
     qt_label_set_alignment

     ;; Push Button
     qt_push_button_create qt_push_button_set_text qt_push_button_text
     raw_qt_push_button_on_clicked

     ;; Line Edit
     qt_line_edit_create qt_line_edit_set_text qt_line_edit_text
     qt_line_edit_set_placeholder qt_line_edit_set_read_only
     qt_line_edit_set_echo_mode
     raw_qt_line_edit_on_text_changed raw_qt_line_edit_on_return_pressed

     ;; Check Box
     qt_check_box_create qt_check_box_set_text
     qt_check_box_set_checked qt_check_box_is_checked
     raw_qt_check_box_on_toggled

     ;; Combo Box
     qt_combo_box_create qt_combo_box_add_item
     qt_combo_box_set_current_index qt_combo_box_current_index
     qt_combo_box_current_text qt_combo_box_count qt_combo_box_clear
     raw_qt_combo_box_on_current_index_changed

     ;; Text Edit
     qt_text_edit_create qt_text_edit_set_text qt_text_edit_text
     qt_text_edit_set_placeholder qt_text_edit_set_read_only
     qt_text_edit_append qt_text_edit_clear
     raw_qt_text_edit_on_text_changed

     ;; Spin Box
     qt_spin_box_create qt_spin_box_set_value qt_spin_box_value
     qt_spin_box_set_range qt_spin_box_set_single_step
     qt_spin_box_set_prefix qt_spin_box_set_suffix
     raw_qt_spin_box_on_value_changed

     ;; Dialog
     qt_dialog_create qt_dialog_exec qt_dialog_accept
     qt_dialog_reject qt_dialog_set_title

     ;; Message Box
     qt_message_box_information qt_message_box_warning
     qt_message_box_question qt_message_box_critical

     ;; File Dialog
     qt_file_dialog_open_file qt_file_dialog_save_file
     qt_file_dialog_open_directory

     ;; Menu Bar
     qt_main_window_menu_bar

     ;; Menu
     qt_menu_bar_add_menu qt_menu_add_menu
     qt_menu_add_action qt_menu_add_separator

     ;; Action
     qt_action_create qt_action_set_text qt_action_text
     qt_action_set_shortcut qt_action_set_enabled qt_action_is_enabled
     qt_action_set_checkable qt_action_is_checkable
     qt_action_set_checked qt_action_is_checked
     qt_action_set_tooltip qt_action_set_status_tip
     raw_qt_action_on_triggered raw_qt_action_on_toggled

     ;; Toolbar
     qt_toolbar_create qt_main_window_add_toolbar
     qt_toolbar_add_action qt_toolbar_add_separator
     qt_toolbar_add_widget qt_toolbar_set_movable
     qt_toolbar_set_icon_size

     ;; Status Bar
     qt_main_window_set_status_bar_text

     ;; List Widget
     qt_list_widget_create qt_list_widget_add_item
     qt_list_widget_insert_item qt_list_widget_remove_item
     qt_list_widget_current_row qt_list_widget_set_current_row
     qt_list_widget_item_text qt_list_widget_count qt_list_widget_clear
     raw_qt_list_widget_on_current_row_changed
     raw_qt_list_widget_on_item_double_clicked

     ;; Table Widget
     qt_table_widget_create qt_table_widget_set_item qt_table_widget_item_text
     qt_table_widget_set_horizontal_header_item
     qt_table_widget_set_vertical_header_item
     qt_table_widget_set_row_count qt_table_widget_set_column_count
     qt_table_widget_row_count qt_table_widget_column_count
     qt_table_widget_current_row qt_table_widget_current_column
     qt_table_widget_clear raw_qt_table_widget_on_cell_clicked

     ;; Tab Widget
     qt_tab_widget_create qt_tab_widget_add_tab
     qt_tab_widget_set_current_index qt_tab_widget_current_index
     qt_tab_widget_count qt_tab_widget_set_tab_text
     raw_qt_tab_widget_on_current_changed

     ;; Progress Bar
     qt_progress_bar_create qt_progress_bar_set_value qt_progress_bar_value
     qt_progress_bar_set_range qt_progress_bar_set_format

     ;; Slider
     qt_slider_create qt_slider_set_value qt_slider_value
     qt_slider_set_range qt_slider_set_single_step
     qt_slider_set_tick_interval qt_slider_set_tick_position
     raw_qt_slider_on_value_changed

     ;; Orientation constants
     QT_HORIZONTAL QT_VERTICAL

     ;; Tick position constants
     QT_TICKS_NONE QT_TICKS_ABOVE QT_TICKS_BELOW QT_TICKS_BOTH_SIDES

     ;; Grid Layout
     qt_grid_layout_create qt_grid_layout_add_widget
     qt_grid_layout_set_row_stretch qt_grid_layout_set_column_stretch
     qt_grid_layout_set_row_minimum_height qt_grid_layout_set_column_minimum_width

     ;; Timer
     qt_timer_create qt_timer_start qt_timer_stop
     qt_timer_set_single_shot qt_timer_is_active
     qt_timer_interval qt_timer_set_interval
     raw_qt_timer_on_timeout raw_qt_timer_single_shot
     qt_timer_destroy

     ;; Clipboard
     qt_clipboard_text qt_clipboard_set_text
     raw_qt_clipboard_on_changed

     ;; Tree Widget
     qt_tree_widget_create qt_tree_widget_set_column_count
     qt_tree_widget_column_count qt_tree_widget_set_header_label
     qt_tree_widget_set_header_item_text
     qt_tree_widget_add_top_level_item qt_tree_widget_top_level_item_count
     qt_tree_widget_top_level_item qt_tree_widget_current_item
     qt_tree_widget_set_current_item
     qt_tree_widget_expand_item qt_tree_widget_collapse_item
     qt_tree_widget_expand_all qt_tree_widget_collapse_all
     qt_tree_widget_clear
     raw_qt_tree_widget_on_current_item_changed
     raw_qt_tree_widget_on_item_double_clicked
     raw_qt_tree_widget_on_item_expanded
     raw_qt_tree_widget_on_item_collapsed

     ;; Tree Widget Item
     qt_tree_item_create qt_tree_item_set_text qt_tree_item_text
     qt_tree_item_add_child qt_tree_item_child_count
     qt_tree_item_child qt_tree_item_parent
     qt_tree_item_set_expanded qt_tree_item_is_expanded

     ;; App-wide Style Sheet
     qt_application_set_style_sheet

     ;; Window State Management
     qt_widget_show_minimized qt_widget_show_maximized
     qt_widget_show_fullscreen qt_widget_show_normal
     qt_widget_window_state qt_widget_move
     qt_widget_x qt_widget_y qt_widget_width qt_widget_height

     ;; Window state constants
     QT_WINDOW_NO_STATE QT_WINDOW_MINIMIZED
     QT_WINDOW_MAXIMIZED QT_WINDOW_FULL_SCREEN

     ;; Scroll Area
     qt_scroll_area_create qt_scroll_area_set_widget
     qt_scroll_area_set_widget_resizable
     qt_scroll_area_set_horizontal_scrollbar_policy
     qt_scroll_area_set_vertical_scrollbar_policy

     ;; Scrollbar policy constants
     QT_SCROLLBAR_AS_NEEDED QT_SCROLLBAR_ALWAYS_OFF QT_SCROLLBAR_ALWAYS_ON

     ;; Splitter
     qt_splitter_create qt_splitter_add_widget qt_splitter_count
     qt_splitter_set_sizes_2 qt_splitter_set_sizes_3
     qt_splitter_size_at qt_splitter_set_stretch_factor
     qt_splitter_set_handle_width
     qt_splitter_set_collapsible qt_splitter_is_collapsible

     ;; Keyboard Events
     raw_qt_widget_install_key_handler
     qt_last_key_code qt_last_key_modifiers qt_last_key_text

     ;; Key constants
     QT_KEY_A QT_KEY_B QT_KEY_C QT_KEY_D QT_KEY_E QT_KEY_F
     QT_KEY_G QT_KEY_H QT_KEY_I QT_KEY_J QT_KEY_K QT_KEY_L
     QT_KEY_M QT_KEY_N QT_KEY_O QT_KEY_P QT_KEY_Q QT_KEY_R
     QT_KEY_S QT_KEY_T QT_KEY_U QT_KEY_V QT_KEY_W QT_KEY_X
     QT_KEY_Y QT_KEY_Z
     QT_KEY_0 QT_KEY_1 QT_KEY_2 QT_KEY_3 QT_KEY_4
     QT_KEY_5 QT_KEY_6 QT_KEY_7 QT_KEY_8 QT_KEY_9
     QT_KEY_ESCAPE QT_KEY_TAB QT_KEY_BACKSPACE QT_KEY_RETURN
     QT_KEY_ENTER QT_KEY_INSERT QT_KEY_DELETE QT_KEY_SPACE
     QT_KEY_UP QT_KEY_DOWN QT_KEY_LEFT QT_KEY_RIGHT
     QT_KEY_HOME QT_KEY_END QT_KEY_PAGE_UP QT_KEY_PAGE_DOWN
     QT_KEY_F1 QT_KEY_F2 QT_KEY_F3 QT_KEY_F4 QT_KEY_F5 QT_KEY_F6
     QT_KEY_F7 QT_KEY_F8 QT_KEY_F9 QT_KEY_F10 QT_KEY_F11 QT_KEY_F12

     ;; Modifier constants
     QT_MOD_NONE QT_MOD_SHIFT QT_MOD_CTRL QT_MOD_ALT QT_MOD_META

     ;; Pixmap
     qt_pixmap_load qt_pixmap_width qt_pixmap_height
     qt_pixmap_is_null qt_pixmap_scaled qt_pixmap_destroy
     qt_label_set_pixmap

     ;; Icon
     qt_icon_create qt_icon_create_from_pixmap
     qt_icon_is_null qt_icon_destroy
     qt_push_button_set_icon qt_action_set_icon
     qt_widget_set_window_icon

     ;; Radio Button
     qt_radio_button_create qt_radio_button_set_text qt_radio_button_text
     qt_radio_button_set_checked qt_radio_button_is_checked
     raw_qt_radio_button_on_toggled

     ;; Button Group
     qt_button_group_create qt_button_group_add_button
     qt_button_group_remove_button qt_button_group_checked_id
     qt_button_group_set_exclusive qt_button_group_is_exclusive
     raw_qt_button_group_on_id_clicked qt_button_group_destroy

     ;; Group Box
     qt_group_box_create qt_group_box_set_title qt_group_box_title
     qt_group_box_set_checkable qt_group_box_is_checkable
     qt_group_box_set_checked qt_group_box_is_checked
     raw_qt_group_box_on_toggled

     ;; Font
     qt_font_create qt_font_family qt_font_point_size
     qt_font_set_bold qt_font_is_bold
     qt_font_set_italic qt_font_is_italic
     qt_font_destroy qt_widget_set_font qt_widget_font

     ;; Color
     qt_color_create_rgb qt_color_create_name
     qt_color_red qt_color_green qt_color_blue qt_color_alpha
     qt_color_name qt_color_is_valid qt_color_destroy

     ;; Font Dialog
     qt_font_dialog_get_font

     ;; Color Dialog
     qt_color_dialog_get_color

     ;; Stacked Widget
     qt_stacked_widget_create qt_stacked_widget_add_widget
     qt_stacked_widget_set_current_index qt_stacked_widget_current_index
     qt_stacked_widget_count raw_qt_stacked_widget_on_current_changed

     ;; Dock Widget
     qt_dock_widget_create qt_dock_widget_set_widget qt_dock_widget_widget
     qt_dock_widget_set_title qt_dock_widget_title
     qt_dock_widget_set_floating qt_dock_widget_is_floating
     qt_main_window_add_dock_widget

     ;; Dock area constants
     QT_DOCK_LEFT QT_DOCK_RIGHT QT_DOCK_TOP QT_DOCK_BOTTOM

     ;; System Tray Icon
     qt_system_tray_icon_create qt_system_tray_icon_set_tooltip
     qt_system_tray_icon_set_icon qt_system_tray_icon_show
     qt_system_tray_icon_hide qt_system_tray_icon_show_message
     qt_system_tray_icon_set_context_menu
     raw_qt_system_tray_icon_on_activated
     qt_system_tray_icon_is_available qt_system_tray_icon_destroy

     ;; Tray icon constants
     QT_TRAY_NO_ICON QT_TRAY_INFO QT_TRAY_WARNING QT_TRAY_CRITICAL
     QT_TRAY_TRIGGER QT_TRAY_CONTEXT QT_TRAY_DOUBLE_CLICK
     QT_TRAY_MIDDLE_CLICK

     ;; QPainter
     qt_pixmap_create_blank qt_pixmap_fill
     qt_painter_create qt_painter_end qt_painter_destroy
     qt_painter_set_pen_color qt_painter_set_pen_width
     qt_painter_set_brush_color qt_painter_set_font_painter
     qt_painter_set_antialiasing
     qt_painter_draw_line qt_painter_draw_rect qt_painter_fill_rect
     qt_painter_draw_ellipse qt_painter_draw_text qt_painter_draw_text_rect
     qt_painter_draw_pixmap qt_painter_draw_point qt_painter_draw_arc
     qt_painter_save qt_painter_restore
     qt_painter_translate qt_painter_rotate qt_painter_scale

     ;; Drag and Drop
     qt_widget_set_accept_drops
     raw_qt_drop_filter_install qt_drop_filter_last_text
     qt_drop_filter_destroy qt_drag_text

     ;; Double Spin Box
     qt_double_spin_box_create qt_double_spin_box_set_value
     qt_double_spin_box_value qt_double_spin_box_set_range
     qt_double_spin_box_set_single_step qt_double_spin_box_set_decimals
     qt_double_spin_box_decimals qt_double_spin_box_set_prefix
     qt_double_spin_box_set_suffix raw_qt_double_spin_box_on_value_changed

     ;; Date Edit
     qt_date_edit_create qt_date_edit_set_date
     qt_date_edit_year qt_date_edit_month qt_date_edit_day
     qt_date_edit_date_string qt_date_edit_set_minimum_date
     qt_date_edit_set_maximum_date qt_date_edit_set_calendar_popup
     qt_date_edit_set_display_format raw_qt_date_edit_on_date_changed

     ;; Time Edit
     qt_time_edit_create qt_time_edit_set_time
     qt_time_edit_hour qt_time_edit_minute qt_time_edit_second
     qt_time_edit_time_string qt_time_edit_set_display_format
     raw_qt_time_edit_on_time_changed

     ;; Frame
     qt_frame_create qt_frame_set_frame_shape qt_frame_frame_shape
     qt_frame_set_frame_shadow qt_frame_frame_shadow
     qt_frame_set_line_width qt_frame_line_width
     qt_frame_set_mid_line_width

     ;; Frame constants
     QT_FRAME_NO_FRAME QT_FRAME_BOX QT_FRAME_PANEL
     QT_FRAME_WIN_PANEL QT_FRAME_HLINE QT_FRAME_VLINE
     QT_FRAME_STYLED_PANEL
     QT_FRAME_PLAIN QT_FRAME_RAISED QT_FRAME_SUNKEN

     ;; Progress Dialog
     qt_progress_dialog_create qt_progress_dialog_set_value
     qt_progress_dialog_value qt_progress_dialog_set_range
     qt_progress_dialog_set_label_text qt_progress_dialog_was_canceled
     qt_progress_dialog_set_minimum_duration
     qt_progress_dialog_set_auto_close qt_progress_dialog_set_auto_reset
     qt_progress_dialog_reset raw_qt_progress_dialog_on_canceled

     ;; Input Dialog
     qt_input_dialog_get_text qt_input_dialog_get_int
     qt_input_dialog_get_double qt_input_dialog_get_item
     qt_input_dialog_was_accepted

     ;; Callback management
     *qt-void-handlers* *qt-string-handlers*
     *qt-int-handlers* *qt-bool-handlers*
     *qt-next-callback-id*
     register-qt-void-handler! register-qt-string-handler!
     register-qt-int-handler! register-qt-bool-handler!)

  (declare (not safe))

  (c-declare #<<END-C
#include "qt_shim.h"

/* Forward declarations for Scheme trampolines (defined by c-define below).
   Note: char* (not const char*) to match Gambit c-define UTF-8-string type. */
void ffi_qt_callback_void(long callback_id);
void ffi_qt_callback_string(long callback_id, char* value);
void ffi_qt_callback_int(long callback_id, int value);
void ffi_qt_callback_bool(long callback_id, int value);

/* ---- Static trampolines (same compilation unit as c-define) ---- */
static void ffi_void_trampoline(long callback_id) {
    ffi_qt_callback_void(callback_id);
}
static void ffi_string_trampoline(long callback_id, const char* value) {
    ffi_qt_callback_string(callback_id, (char*)value);
}
static void ffi_int_trampoline(long callback_id, int value) {
    ffi_qt_callback_int(callback_id, value);
}
static void ffi_bool_trampoline(long callback_id, int value) {
    ffi_qt_callback_bool(callback_id, value);
}

/* ---- C wrappers ---- */

/* Create QApplication with no args (uses internal defaults) */
static void* ffi_qt_application_create(void) {
    return qt_application_create(0, NULL);
}

/* Connect button clicked signal using our static trampoline */
static void ffi_qt_push_button_on_clicked(void* b, long callback_id) {
    qt_push_button_on_clicked(b, ffi_void_trampoline, callback_id);
}

/* String getters — cast away const to suppress Gambit ___CFUN_ASSIGN warning */
static char* ffi_qt_label_text(void* l) {
    return (char*)qt_label_text(l);
}
static char* ffi_qt_push_button_text(void* b) {
    return (char*)qt_push_button_text(b);
}

/* ---- Phase 2: signal connection wrappers ---- */

/* Line Edit */
static void ffi_qt_line_edit_on_text_changed(void* e, long callback_id) {
    qt_line_edit_on_text_changed(e, ffi_string_trampoline, callback_id);
}
static void ffi_qt_line_edit_on_return_pressed(void* e, long callback_id) {
    qt_line_edit_on_return_pressed(e, ffi_void_trampoline, callback_id);
}
static char* ffi_qt_line_edit_text(void* e) {
    return (char*)qt_line_edit_text(e);
}

/* Check Box */
static void ffi_qt_check_box_on_toggled(void* c, long callback_id) {
    qt_check_box_on_toggled(c, ffi_bool_trampoline, callback_id);
}

/* Combo Box */
static void ffi_qt_combo_box_on_current_index_changed(void* c, long callback_id) {
    qt_combo_box_on_current_index_changed(c, ffi_int_trampoline, callback_id);
}
static char* ffi_qt_combo_box_current_text(void* c) {
    return (char*)qt_combo_box_current_text(c);
}

/* Text Edit */
static void ffi_qt_text_edit_on_text_changed(void* e, long callback_id) {
    qt_text_edit_on_text_changed(e, ffi_void_trampoline, callback_id);
}
static char* ffi_qt_text_edit_text(void* e) {
    return (char*)qt_text_edit_text(e);
}

/* Spin Box */
static void ffi_qt_spin_box_on_value_changed(void* s, long callback_id) {
    qt_spin_box_on_value_changed(s, ffi_int_trampoline, callback_id);
}

/* File Dialog — cast away const */
static char* ffi_qt_file_dialog_open_file(void* parent, const char* caption,
                                           const char* dir, const char* filter) {
    return (char*)qt_file_dialog_open_file(parent, caption, dir, filter);
}
static char* ffi_qt_file_dialog_save_file(void* parent, const char* caption,
                                           const char* dir, const char* filter) {
    return (char*)qt_file_dialog_save_file(parent, caption, dir, filter);
}
static char* ffi_qt_file_dialog_open_directory(void* parent, const char* caption,
                                                const char* dir) {
    return (char*)qt_file_dialog_open_directory(parent, caption, dir);
}

/* ---- Phase 3: menu/action/toolbar wrappers ---- */

/* Action */
static void ffi_qt_action_on_triggered(void* a, long callback_id) {
    qt_action_on_triggered(a, ffi_void_trampoline, callback_id);
}
static void ffi_qt_action_on_toggled(void* a, long callback_id) {
    qt_action_on_toggled(a, ffi_bool_trampoline, callback_id);
}
static char* ffi_qt_action_text(void* a) {
    return (char*)qt_action_text(a);
}

/* ---- Phase 4: list/table/tab/progress/slider wrappers ---- */

/* List Widget */
static void ffi_qt_list_widget_on_current_row_changed(void* l, long callback_id) {
    qt_list_widget_on_current_row_changed(l, ffi_int_trampoline, callback_id);
}
static void ffi_qt_list_widget_on_item_double_clicked(void* l, long callback_id) {
    qt_list_widget_on_item_double_clicked(l, ffi_int_trampoline, callback_id);
}
static char* ffi_qt_list_widget_item_text(void* l, int row) {
    return (char*)qt_list_widget_item_text(l, row);
}

/* Table Widget */
static void ffi_qt_table_widget_on_cell_clicked(void* t, long callback_id) {
    qt_table_widget_on_cell_clicked(t, ffi_void_trampoline, callback_id);
}
static char* ffi_qt_table_widget_item_text(void* t, int row, int col) {
    return (char*)qt_table_widget_item_text(t, row, col);
}

/* Tab Widget */
static void ffi_qt_tab_widget_on_current_changed(void* t, long callback_id) {
    qt_tab_widget_on_current_changed(t, ffi_int_trampoline, callback_id);
}

/* Slider */
static void ffi_qt_slider_on_value_changed(void* s, long callback_id) {
    qt_slider_on_value_changed(s, ffi_int_trampoline, callback_id);
}

/* ---- Phase 5: grid/timer/clipboard/tree wrappers ---- */

/* Timer */
static void ffi_qt_timer_on_timeout(void* t, long callback_id) {
    qt_timer_on_timeout(t, ffi_void_trampoline, callback_id);
}
static void ffi_qt_timer_single_shot(int msec, long callback_id) {
    qt_timer_single_shot(msec, ffi_void_trampoline, callback_id);
}

/* Clipboard */
static char* ffi_qt_clipboard_text(void* app) {
    return (char*)qt_clipboard_text(app);
}
static void ffi_qt_clipboard_on_changed(void* app, long callback_id) {
    qt_clipboard_on_changed(app, ffi_void_trampoline, callback_id);
}

/* Tree Widget signals */
static void ffi_qt_tree_widget_on_current_item_changed(void* t, long callback_id) {
    qt_tree_widget_on_current_item_changed(t, ffi_void_trampoline, callback_id);
}
static void ffi_qt_tree_widget_on_item_double_clicked(void* t, long callback_id) {
    qt_tree_widget_on_item_double_clicked(t, ffi_void_trampoline, callback_id);
}
static void ffi_qt_tree_widget_on_item_expanded(void* t, long callback_id) {
    qt_tree_widget_on_item_expanded(t, ffi_void_trampoline, callback_id);
}
static void ffi_qt_tree_widget_on_item_collapsed(void* t, long callback_id) {
    qt_tree_widget_on_item_collapsed(t, ffi_void_trampoline, callback_id);
}

/* Tree Item — cast away const */
static char* ffi_qt_tree_item_text(void* item, int col) {
    return (char*)qt_tree_item_text(item, col);
}

/* Alignment constants (Qt::AlignmentFlag values) */
#define QT_ALIGN_LEFT    0x0001
#define QT_ALIGN_RIGHT   0x0002
#define QT_ALIGN_CENTER  0x0084
#define QT_ALIGN_TOP     0x0020
#define QT_ALIGN_BOTTOM  0x0040

/* Echo mode constants (QLineEdit::EchoMode values) */
#define QT_ECHO_NORMAL          0
#define QT_ECHO_NO_ECHO         1
#define QT_ECHO_PASSWORD        2
#define QT_ECHO_PASSWORD_ON_EDIT 3

/* Orientation constants (Qt::Orientation) */
#define QT_HORIZONTAL  0x1
#define QT_VERTICAL    0x2

/* Slider tick position constants (QSlider::TickPosition) */
#define QT_TICKS_NONE        0
#define QT_TICKS_ABOVE       1
#define QT_TICKS_BELOW       2
#define QT_TICKS_BOTH_SIDES  3

/* Window state constants (Qt::WindowState) */
#define QT_WINDOW_NO_STATE    0x00
#define QT_WINDOW_MINIMIZED   0x01
#define QT_WINDOW_MAXIMIZED   0x02
#define QT_WINDOW_FULL_SCREEN 0x04

/* Scrollbar policy constants (Qt::ScrollBarPolicy) */
#define QT_SCROLLBAR_AS_NEEDED  0
#define QT_SCROLLBAR_ALWAYS_OFF 1
#define QT_SCROLLBAR_ALWAYS_ON  2

/* Key constants (Qt::Key) */
#define QT_KEY_A         0x41
#define QT_KEY_B         0x42
#define QT_KEY_C         0x43
#define QT_KEY_D         0x44
#define QT_KEY_E         0x45
#define QT_KEY_F         0x46
#define QT_KEY_G         0x47
#define QT_KEY_H         0x48
#define QT_KEY_I         0x49
#define QT_KEY_J         0x4a
#define QT_KEY_K         0x4b
#define QT_KEY_L         0x4c
#define QT_KEY_M         0x4d
#define QT_KEY_N         0x4e
#define QT_KEY_O         0x4f
#define QT_KEY_P         0x50
#define QT_KEY_Q         0x51
#define QT_KEY_R         0x52
#define QT_KEY_S         0x53
#define QT_KEY_T         0x54
#define QT_KEY_U         0x55
#define QT_KEY_V         0x56
#define QT_KEY_W         0x57
#define QT_KEY_X         0x58
#define QT_KEY_Y         0x59
#define QT_KEY_Z         0x5a
#define QT_KEY_0         0x30
#define QT_KEY_1         0x31
#define QT_KEY_2         0x32
#define QT_KEY_3         0x33
#define QT_KEY_4         0x34
#define QT_KEY_5         0x35
#define QT_KEY_6         0x36
#define QT_KEY_7         0x37
#define QT_KEY_8         0x38
#define QT_KEY_9         0x39
#define QT_KEY_ESCAPE    0x01000000
#define QT_KEY_TAB       0x01000001
#define QT_KEY_BACKSPACE 0x01000003
#define QT_KEY_RETURN    0x01000004
#define QT_KEY_ENTER     0x01000005
#define QT_KEY_INSERT    0x01000006
#define QT_KEY_DELETE    0x01000007
#define QT_KEY_SPACE     0x20
#define QT_KEY_UP        0x01000013
#define QT_KEY_DOWN      0x01000015
#define QT_KEY_LEFT      0x01000012
#define QT_KEY_RIGHT     0x01000014
#define QT_KEY_HOME      0x01000010
#define QT_KEY_END       0x01000011
#define QT_KEY_PAGE_UP   0x01000016
#define QT_KEY_PAGE_DOWN 0x01000017
#define QT_KEY_F1        0x01000030
#define QT_KEY_F2        0x01000031
#define QT_KEY_F3        0x01000032
#define QT_KEY_F4        0x01000033
#define QT_KEY_F5        0x01000034
#define QT_KEY_F6        0x01000035
#define QT_KEY_F7        0x01000036
#define QT_KEY_F8        0x01000037
#define QT_KEY_F9        0x01000038
#define QT_KEY_F10       0x01000039
#define QT_KEY_F11       0x0100003a
#define QT_KEY_F12       0x0100003b

/* Modifier constants (Qt::KeyboardModifier) */
#define QT_MOD_NONE      0x00000000
#define QT_MOD_SHIFT     0x02000000
#define QT_MOD_CTRL      0x04000000
#define QT_MOD_ALT       0x08000000
#define QT_MOD_META      0x10000000

/* ---- Phase 6: wrappers ---- */

/* Key handler — uses void trampoline */
static void ffi_qt_widget_install_key_handler(void* w, long callback_id) {
    qt_widget_install_key_handler(w, ffi_void_trampoline, callback_id);
}

/* Key text — cast away const */
static char* ffi_qt_last_key_text(void) {
    return (char*)qt_last_key_text();
}

/* ---- Phase 7: pixmap/icon/radio/buttongroup/groupbox wrappers ---- */

/* Radio Button */
static void ffi_qt_radio_button_on_toggled(void* r, long callback_id) {
    qt_radio_button_on_toggled(r, ffi_bool_trampoline, callback_id);
}
static char* ffi_qt_radio_button_text(void* r) {
    return (char*)qt_radio_button_text(r);
}

/* Button Group */
static void ffi_qt_button_group_on_id_clicked(void* bg, long callback_id) {
    qt_button_group_on_id_clicked(bg, ffi_int_trampoline, callback_id);
}

/* Group Box */
static void ffi_qt_group_box_on_toggled(void* gb, long callback_id) {
    qt_group_box_on_toggled(gb, ffi_bool_trampoline, callback_id);
}
static char* ffi_qt_group_box_title(void* gb) {
    return (char*)qt_group_box_title(gb);
}

/* ---- Phase 8a: font/color wrappers ---- */

/* Font — cast away const */
static char* ffi_qt_font_family(void* f) {
    return (char*)qt_font_family(f);
}

/* Color — cast away const */
static char* ffi_qt_color_name(void* c) {
    return (char*)qt_color_name(c);
}

/* ---- Phase 8b: stacked/dock wrappers ---- */

/* Stacked Widget */
static void ffi_qt_stacked_widget_on_current_changed(void* sw, long callback_id) {
    qt_stacked_widget_on_current_changed(sw, ffi_int_trampoline, callback_id);
}

/* Dock Widget — cast away const */
static char* ffi_qt_dock_widget_title(void* dw) {
    return (char*)qt_dock_widget_title(dw);
}

/* Dock area constants (Qt::DockWidgetArea) */
#define QT_DOCK_LEFT    1
#define QT_DOCK_RIGHT   2
#define QT_DOCK_TOP     4
#define QT_DOCK_BOTTOM  8

/* ---- Phase 8c: system tray wrappers ---- */

static void ffi_qt_system_tray_icon_on_activated(void* ti, long callback_id) {
    qt_system_tray_icon_on_activated(ti, ffi_int_trampoline, callback_id);
}

/* Tray message icon constants (QSystemTrayIcon::MessageIcon) */
#define QT_TRAY_NO_ICON    0
#define QT_TRAY_INFO       1
#define QT_TRAY_WARNING    2
#define QT_TRAY_CRITICAL   3

/* Tray activation reason constants (QSystemTrayIcon::ActivationReason) */
#define QT_TRAY_TRIGGER       3
#define QT_TRAY_CONTEXT       1
#define QT_TRAY_DOUBLE_CLICK  2
#define QT_TRAY_MIDDLE_CLICK  4

/* ---- Phase 8e: drag and drop wrappers ---- */

static void* ffi_qt_drop_filter_install(void* widget, long callback_id) {
    return qt_drop_filter_install(widget, ffi_string_trampoline, callback_id);
}

static char* ffi_qt_drop_filter_last_text(void* df) {
    return (char*)qt_drop_filter_last_text(df);
}

/* ---- Phase 9: double spin box / date edit / time edit / frame / progress dialog / input dialog wrappers ---- */

/* Double Spin Box — value changed uses string trampoline (double → string) */
static void ffi_qt_double_spin_box_on_value_changed(void* s, long callback_id) {
    qt_double_spin_box_on_value_changed(s, ffi_string_trampoline, callback_id);
}

/* Date Edit */
static void ffi_qt_date_edit_on_date_changed(void* d, long callback_id) {
    qt_date_edit_on_date_changed(d, ffi_string_trampoline, callback_id);
}
static char* ffi_qt_date_edit_date_string(void* d) {
    return (char*)qt_date_edit_date_string(d);
}

/* Time Edit */
static void ffi_qt_time_edit_on_time_changed(void* t, long callback_id) {
    qt_time_edit_on_time_changed(t, ffi_string_trampoline, callback_id);
}
static char* ffi_qt_time_edit_time_string(void* t) {
    return (char*)qt_time_edit_time_string(t);
}

/* Progress Dialog — canceled uses void trampoline */
static void ffi_qt_progress_dialog_on_canceled(void* pd, long callback_id) {
    qt_progress_dialog_on_canceled(pd, ffi_void_trampoline, callback_id);
}

/* Input Dialog — cast away const */
static char* ffi_qt_input_dialog_get_text(void* parent, const char* title,
                                           const char* label, const char* default_text) {
    return (char*)qt_input_dialog_get_text(parent, title, label, default_text);
}
static char* ffi_qt_input_dialog_get_item(void* parent, const char* title,
                                           const char* label, const char* items_newline,
                                           int current, int editable) {
    return (char*)qt_input_dialog_get_item(parent, title, label,
                                            items_newline, current, editable);
}

/* Frame constants */
#define QT_FRAME_NO_FRAME     0
#define QT_FRAME_BOX          1
#define QT_FRAME_PANEL        2
#define QT_FRAME_WIN_PANEL    3
#define QT_FRAME_HLINE        4
#define QT_FRAME_VLINE        5
#define QT_FRAME_STYLED_PANEL 6
#define QT_FRAME_PLAIN        0x0010
#define QT_FRAME_RAISED       0x0020
#define QT_FRAME_SUNKEN       0x0030

END-C
  )

  ;; ---- Constants ----
  (define-const QT_ALIGN_LEFT)
  (define-const QT_ALIGN_RIGHT)
  (define-const QT_ALIGN_CENTER)
  (define-const QT_ALIGN_TOP)
  (define-const QT_ALIGN_BOTTOM)

  (define-const QT_ECHO_NORMAL)
  (define-const QT_ECHO_NO_ECHO)
  (define-const QT_ECHO_PASSWORD)
  (define-const QT_ECHO_PASSWORD_ON_EDIT)

  (define-const QT_HORIZONTAL)
  (define-const QT_VERTICAL)

  (define-const QT_TICKS_NONE)
  (define-const QT_TICKS_ABOVE)
  (define-const QT_TICKS_BELOW)
  (define-const QT_TICKS_BOTH_SIDES)

  ;; Window state constants
  (define-const QT_WINDOW_NO_STATE)
  (define-const QT_WINDOW_MINIMIZED)
  (define-const QT_WINDOW_MAXIMIZED)
  (define-const QT_WINDOW_FULL_SCREEN)

  ;; Scrollbar policy constants
  (define-const QT_SCROLLBAR_AS_NEEDED)
  (define-const QT_SCROLLBAR_ALWAYS_OFF)
  (define-const QT_SCROLLBAR_ALWAYS_ON)

  ;; Key constants
  (define-const QT_KEY_A)
  (define-const QT_KEY_B)
  (define-const QT_KEY_C)
  (define-const QT_KEY_D)
  (define-const QT_KEY_E)
  (define-const QT_KEY_F)
  (define-const QT_KEY_G)
  (define-const QT_KEY_H)
  (define-const QT_KEY_I)
  (define-const QT_KEY_J)
  (define-const QT_KEY_K)
  (define-const QT_KEY_L)
  (define-const QT_KEY_M)
  (define-const QT_KEY_N)
  (define-const QT_KEY_O)
  (define-const QT_KEY_P)
  (define-const QT_KEY_Q)
  (define-const QT_KEY_R)
  (define-const QT_KEY_S)
  (define-const QT_KEY_T)
  (define-const QT_KEY_U)
  (define-const QT_KEY_V)
  (define-const QT_KEY_W)
  (define-const QT_KEY_X)
  (define-const QT_KEY_Y)
  (define-const QT_KEY_Z)
  (define-const QT_KEY_0)
  (define-const QT_KEY_1)
  (define-const QT_KEY_2)
  (define-const QT_KEY_3)
  (define-const QT_KEY_4)
  (define-const QT_KEY_5)
  (define-const QT_KEY_6)
  (define-const QT_KEY_7)
  (define-const QT_KEY_8)
  (define-const QT_KEY_9)
  (define-const QT_KEY_ESCAPE)
  (define-const QT_KEY_TAB)
  (define-const QT_KEY_BACKSPACE)
  (define-const QT_KEY_RETURN)
  (define-const QT_KEY_ENTER)
  (define-const QT_KEY_INSERT)
  (define-const QT_KEY_DELETE)
  (define-const QT_KEY_SPACE)
  (define-const QT_KEY_UP)
  (define-const QT_KEY_DOWN)
  (define-const QT_KEY_LEFT)
  (define-const QT_KEY_RIGHT)
  (define-const QT_KEY_HOME)
  (define-const QT_KEY_END)
  (define-const QT_KEY_PAGE_UP)
  (define-const QT_KEY_PAGE_DOWN)
  (define-const QT_KEY_F1)
  (define-const QT_KEY_F2)
  (define-const QT_KEY_F3)
  (define-const QT_KEY_F4)
  (define-const QT_KEY_F5)
  (define-const QT_KEY_F6)
  (define-const QT_KEY_F7)
  (define-const QT_KEY_F8)
  (define-const QT_KEY_F9)
  (define-const QT_KEY_F10)
  (define-const QT_KEY_F11)
  (define-const QT_KEY_F12)

  ;; Modifier constants
  (define-const QT_MOD_NONE)
  (define-const QT_MOD_SHIFT)
  (define-const QT_MOD_CTRL)
  (define-const QT_MOD_ALT)
  (define-const QT_MOD_META)

  ;; Dock area constants
  (define-const QT_DOCK_LEFT)
  (define-const QT_DOCK_RIGHT)
  (define-const QT_DOCK_TOP)
  (define-const QT_DOCK_BOTTOM)

  ;; Tray message icon constants
  (define-const QT_TRAY_NO_ICON)
  (define-const QT_TRAY_INFO)
  (define-const QT_TRAY_WARNING)
  (define-const QT_TRAY_CRITICAL)

  ;; Tray activation reason constants
  (define-const QT_TRAY_TRIGGER)
  (define-const QT_TRAY_CONTEXT)
  (define-const QT_TRAY_DOUBLE_CLICK)
  (define-const QT_TRAY_MIDDLE_CLICK)

  ;; ---- Application lifecycle ----
  (define-c-lambda qt_application_create () (pointer void)
    "ffi_qt_application_create")
  (define-c-lambda qt_application_exec ((pointer void)) int
    "qt_application_exec")
  (define-c-lambda qt_application_quit ((pointer void)) void
    "qt_application_quit")
  (define-c-lambda qt_application_process_events ((pointer void)) void
    "qt_application_process_events")
  (define-c-lambda qt_application_destroy ((pointer void)) void
    "qt_application_destroy")

  ;; ---- Widget base ----
  (define-c-lambda qt_widget_create ((pointer void)) (pointer void)
    "qt_widget_create")
  (define-c-lambda qt_widget_show ((pointer void)) void
    "qt_widget_show")
  (define-c-lambda qt_widget_hide ((pointer void)) void
    "qt_widget_hide")
  (define-c-lambda qt_widget_close ((pointer void)) void
    "qt_widget_close")
  (define-c-lambda qt_widget_set_enabled ((pointer void) int) void
    "qt_widget_set_enabled")
  (define-c-lambda qt_widget_is_enabled ((pointer void)) int
    "qt_widget_is_enabled")
  (define-c-lambda qt_widget_set_visible ((pointer void) int) void
    "qt_widget_set_visible")
  (define-c-lambda qt_widget_set_fixed_size ((pointer void) int int) void
    "qt_widget_set_fixed_size")
  (define-c-lambda qt_widget_set_minimum_size ((pointer void) int int) void
    "qt_widget_set_minimum_size")
  (define-c-lambda qt_widget_set_maximum_size ((pointer void) int int) void
    "qt_widget_set_maximum_size")
  (define-c-lambda qt_widget_resize ((pointer void) int int) void
    "qt_widget_resize")
  (define-c-lambda qt_widget_set_style_sheet ((pointer void) UTF-8-string) void
    "qt_widget_set_style_sheet")
  (define-c-lambda qt_widget_set_tooltip ((pointer void) UTF-8-string) void
    "qt_widget_set_tooltip")
  (define-c-lambda qt_widget_set_font_size ((pointer void) int) void
    "qt_widget_set_font_size")
  (define-c-lambda qt_widget_destroy ((pointer void)) void
    "qt_widget_destroy")

  ;; ---- Main Window ----
  (define-c-lambda qt_main_window_create ((pointer void)) (pointer void)
    "qt_main_window_create")
  (define-c-lambda qt_main_window_set_title ((pointer void) UTF-8-string) void
    "qt_main_window_set_title")
  (define-c-lambda qt_main_window_set_central_widget
    ((pointer void) (pointer void)) void
    "qt_main_window_set_central_widget")

  ;; ---- Layouts ----
  (define-c-lambda qt_vbox_layout_create ((pointer void)) (pointer void)
    "qt_vbox_layout_create")
  (define-c-lambda qt_hbox_layout_create ((pointer void)) (pointer void)
    "qt_hbox_layout_create")
  (define-c-lambda qt_layout_add_widget ((pointer void) (pointer void)) void
    "qt_layout_add_widget")
  (define-c-lambda qt_layout_add_stretch ((pointer void) int) void
    "qt_layout_add_stretch")
  (define-c-lambda qt_layout_set_spacing ((pointer void) int) void
    "qt_layout_set_spacing")
  (define-c-lambda qt_layout_set_margins ((pointer void) int int int int) void
    "qt_layout_set_margins")

  ;; ---- Labels ----
  (define-c-lambda qt_label_create (UTF-8-string (pointer void)) (pointer void)
    "qt_label_create")
  (define-c-lambda qt_label_set_text ((pointer void) UTF-8-string) void
    "qt_label_set_text")
  (define-c-lambda qt_label_text ((pointer void)) UTF-8-string
    "ffi_qt_label_text")
  (define-c-lambda qt_label_set_alignment ((pointer void) int) void
    "qt_label_set_alignment")

  ;; ---- Push Button ----
  (define-c-lambda qt_push_button_create
    (UTF-8-string (pointer void)) (pointer void)
    "qt_push_button_create")
  (define-c-lambda qt_push_button_set_text ((pointer void) UTF-8-string) void
    "qt_push_button_set_text")
  (define-c-lambda qt_push_button_text ((pointer void)) UTF-8-string
    "ffi_qt_push_button_text")
  (define-c-lambda raw_qt_push_button_on_clicked ((pointer void) long) void
    "ffi_qt_push_button_on_clicked")

  ;; ---- Line Edit ----
  (define-c-lambda qt_line_edit_create ((pointer void)) (pointer void)
    "qt_line_edit_create")
  (define-c-lambda qt_line_edit_set_text ((pointer void) UTF-8-string) void
    "qt_line_edit_set_text")
  (define-c-lambda qt_line_edit_text ((pointer void)) UTF-8-string
    "ffi_qt_line_edit_text")
  (define-c-lambda qt_line_edit_set_placeholder ((pointer void) UTF-8-string) void
    "qt_line_edit_set_placeholder")
  (define-c-lambda qt_line_edit_set_read_only ((pointer void) int) void
    "qt_line_edit_set_read_only")
  (define-c-lambda qt_line_edit_set_echo_mode ((pointer void) int) void
    "qt_line_edit_set_echo_mode")
  (define-c-lambda raw_qt_line_edit_on_text_changed ((pointer void) long) void
    "ffi_qt_line_edit_on_text_changed")
  (define-c-lambda raw_qt_line_edit_on_return_pressed ((pointer void) long) void
    "ffi_qt_line_edit_on_return_pressed")

  ;; ---- Check Box ----
  (define-c-lambda qt_check_box_create (UTF-8-string (pointer void)) (pointer void)
    "qt_check_box_create")
  (define-c-lambda qt_check_box_set_text ((pointer void) UTF-8-string) void
    "qt_check_box_set_text")
  (define-c-lambda qt_check_box_set_checked ((pointer void) int) void
    "qt_check_box_set_checked")
  (define-c-lambda qt_check_box_is_checked ((pointer void)) int
    "qt_check_box_is_checked")
  (define-c-lambda raw_qt_check_box_on_toggled ((pointer void) long) void
    "ffi_qt_check_box_on_toggled")

  ;; ---- Combo Box ----
  (define-c-lambda qt_combo_box_create ((pointer void)) (pointer void)
    "qt_combo_box_create")
  (define-c-lambda qt_combo_box_add_item ((pointer void) UTF-8-string) void
    "qt_combo_box_add_item")
  (define-c-lambda qt_combo_box_set_current_index ((pointer void) int) void
    "qt_combo_box_set_current_index")
  (define-c-lambda qt_combo_box_current_index ((pointer void)) int
    "qt_combo_box_current_index")
  (define-c-lambda qt_combo_box_current_text ((pointer void)) UTF-8-string
    "ffi_qt_combo_box_current_text")
  (define-c-lambda qt_combo_box_count ((pointer void)) int
    "qt_combo_box_count")
  (define-c-lambda qt_combo_box_clear ((pointer void)) void
    "qt_combo_box_clear")
  (define-c-lambda raw_qt_combo_box_on_current_index_changed ((pointer void) long) void
    "ffi_qt_combo_box_on_current_index_changed")

  ;; ---- Text Edit ----
  (define-c-lambda qt_text_edit_create ((pointer void)) (pointer void)
    "qt_text_edit_create")
  (define-c-lambda qt_text_edit_set_text ((pointer void) UTF-8-string) void
    "qt_text_edit_set_text")
  (define-c-lambda qt_text_edit_text ((pointer void)) UTF-8-string
    "ffi_qt_text_edit_text")
  (define-c-lambda qt_text_edit_set_placeholder ((pointer void) UTF-8-string) void
    "qt_text_edit_set_placeholder")
  (define-c-lambda qt_text_edit_set_read_only ((pointer void) int) void
    "qt_text_edit_set_read_only")
  (define-c-lambda qt_text_edit_append ((pointer void) UTF-8-string) void
    "qt_text_edit_append")
  (define-c-lambda qt_text_edit_clear ((pointer void)) void
    "qt_text_edit_clear")
  (define-c-lambda raw_qt_text_edit_on_text_changed ((pointer void) long) void
    "ffi_qt_text_edit_on_text_changed")

  ;; ---- Spin Box ----
  (define-c-lambda qt_spin_box_create ((pointer void)) (pointer void)
    "qt_spin_box_create")
  (define-c-lambda qt_spin_box_set_value ((pointer void) int) void
    "qt_spin_box_set_value")
  (define-c-lambda qt_spin_box_value ((pointer void)) int
    "qt_spin_box_value")
  (define-c-lambda qt_spin_box_set_range ((pointer void) int int) void
    "qt_spin_box_set_range")
  (define-c-lambda qt_spin_box_set_single_step ((pointer void) int) void
    "qt_spin_box_set_single_step")
  (define-c-lambda qt_spin_box_set_prefix ((pointer void) UTF-8-string) void
    "qt_spin_box_set_prefix")
  (define-c-lambda qt_spin_box_set_suffix ((pointer void) UTF-8-string) void
    "qt_spin_box_set_suffix")
  (define-c-lambda raw_qt_spin_box_on_value_changed ((pointer void) long) void
    "ffi_qt_spin_box_on_value_changed")

  ;; ---- Dialog ----
  (define-c-lambda qt_dialog_create ((pointer void)) (pointer void)
    "qt_dialog_create")
  (define-c-lambda qt_dialog_exec ((pointer void)) int
    "qt_dialog_exec")
  (define-c-lambda qt_dialog_accept ((pointer void)) void
    "qt_dialog_accept")
  (define-c-lambda qt_dialog_reject ((pointer void)) void
    "qt_dialog_reject")
  (define-c-lambda qt_dialog_set_title ((pointer void) UTF-8-string) void
    "qt_dialog_set_title")

  ;; ---- Message Box ----
  (define-c-lambda qt_message_box_information
    ((pointer void) UTF-8-string UTF-8-string) int
    "qt_message_box_information")
  (define-c-lambda qt_message_box_warning
    ((pointer void) UTF-8-string UTF-8-string) int
    "qt_message_box_warning")
  (define-c-lambda qt_message_box_question
    ((pointer void) UTF-8-string UTF-8-string) int
    "qt_message_box_question")
  (define-c-lambda qt_message_box_critical
    ((pointer void) UTF-8-string UTF-8-string) int
    "qt_message_box_critical")

  ;; ---- File Dialog ----
  (define-c-lambda qt_file_dialog_open_file
    ((pointer void) UTF-8-string UTF-8-string UTF-8-string) UTF-8-string
    "ffi_qt_file_dialog_open_file")
  (define-c-lambda qt_file_dialog_save_file
    ((pointer void) UTF-8-string UTF-8-string UTF-8-string) UTF-8-string
    "ffi_qt_file_dialog_save_file")
  (define-c-lambda qt_file_dialog_open_directory
    ((pointer void) UTF-8-string UTF-8-string) UTF-8-string
    "ffi_qt_file_dialog_open_directory")

  ;; ---- Menu Bar ----
  (define-c-lambda qt_main_window_menu_bar ((pointer void)) (pointer void)
    "qt_main_window_menu_bar")

  ;; ---- Menu ----
  (define-c-lambda qt_menu_bar_add_menu ((pointer void) UTF-8-string) (pointer void)
    "qt_menu_bar_add_menu")
  (define-c-lambda qt_menu_add_menu ((pointer void) UTF-8-string) (pointer void)
    "qt_menu_add_menu")
  (define-c-lambda qt_menu_add_action ((pointer void) (pointer void)) void
    "qt_menu_add_action")
  (define-c-lambda qt_menu_add_separator ((pointer void)) void
    "qt_menu_add_separator")

  ;; ---- Action ----
  (define-c-lambda qt_action_create (UTF-8-string (pointer void)) (pointer void)
    "qt_action_create")
  (define-c-lambda qt_action_set_text ((pointer void) UTF-8-string) void
    "qt_action_set_text")
  (define-c-lambda qt_action_text ((pointer void)) UTF-8-string
    "ffi_qt_action_text")
  (define-c-lambda qt_action_set_shortcut ((pointer void) UTF-8-string) void
    "qt_action_set_shortcut")
  (define-c-lambda qt_action_set_enabled ((pointer void) int) void
    "qt_action_set_enabled")
  (define-c-lambda qt_action_is_enabled ((pointer void)) int
    "qt_action_is_enabled")
  (define-c-lambda qt_action_set_checkable ((pointer void) int) void
    "qt_action_set_checkable")
  (define-c-lambda qt_action_is_checkable ((pointer void)) int
    "qt_action_is_checkable")
  (define-c-lambda qt_action_set_checked ((pointer void) int) void
    "qt_action_set_checked")
  (define-c-lambda qt_action_is_checked ((pointer void)) int
    "qt_action_is_checked")
  (define-c-lambda qt_action_set_tooltip ((pointer void) UTF-8-string) void
    "qt_action_set_tooltip")
  (define-c-lambda qt_action_set_status_tip ((pointer void) UTF-8-string) void
    "qt_action_set_status_tip")
  (define-c-lambda raw_qt_action_on_triggered ((pointer void) long) void
    "ffi_qt_action_on_triggered")
  (define-c-lambda raw_qt_action_on_toggled ((pointer void) long) void
    "ffi_qt_action_on_toggled")

  ;; ---- Toolbar ----
  (define-c-lambda qt_toolbar_create (UTF-8-string (pointer void)) (pointer void)
    "qt_toolbar_create")
  (define-c-lambda qt_main_window_add_toolbar ((pointer void) (pointer void)) void
    "qt_main_window_add_toolbar")
  (define-c-lambda qt_toolbar_add_action ((pointer void) (pointer void)) void
    "qt_toolbar_add_action")
  (define-c-lambda qt_toolbar_add_separator ((pointer void)) void
    "qt_toolbar_add_separator")
  (define-c-lambda qt_toolbar_add_widget ((pointer void) (pointer void)) void
    "qt_toolbar_add_widget")
  (define-c-lambda qt_toolbar_set_movable ((pointer void) int) void
    "qt_toolbar_set_movable")
  (define-c-lambda qt_toolbar_set_icon_size ((pointer void) int int) void
    "qt_toolbar_set_icon_size")

  ;; ---- Status Bar ----
  (define-c-lambda qt_main_window_set_status_bar_text
    ((pointer void) UTF-8-string) void
    "qt_main_window_set_status_bar_text")

  ;; ---- List Widget ----
  (define-c-lambda qt_list_widget_create ((pointer void)) (pointer void)
    "qt_list_widget_create")
  (define-c-lambda qt_list_widget_add_item ((pointer void) UTF-8-string) void
    "qt_list_widget_add_item")
  (define-c-lambda qt_list_widget_insert_item ((pointer void) int UTF-8-string) void
    "qt_list_widget_insert_item")
  (define-c-lambda qt_list_widget_remove_item ((pointer void) int) void
    "qt_list_widget_remove_item")
  (define-c-lambda qt_list_widget_current_row ((pointer void)) int
    "qt_list_widget_current_row")
  (define-c-lambda qt_list_widget_set_current_row ((pointer void) int) void
    "qt_list_widget_set_current_row")
  (define-c-lambda qt_list_widget_item_text ((pointer void) int) UTF-8-string
    "ffi_qt_list_widget_item_text")
  (define-c-lambda qt_list_widget_count ((pointer void)) int
    "qt_list_widget_count")
  (define-c-lambda qt_list_widget_clear ((pointer void)) void
    "qt_list_widget_clear")
  (define-c-lambda raw_qt_list_widget_on_current_row_changed
    ((pointer void) long) void
    "ffi_qt_list_widget_on_current_row_changed")
  (define-c-lambda raw_qt_list_widget_on_item_double_clicked
    ((pointer void) long) void
    "ffi_qt_list_widget_on_item_double_clicked")

  ;; ---- Table Widget ----
  (define-c-lambda qt_table_widget_create (int int (pointer void)) (pointer void)
    "qt_table_widget_create")
  (define-c-lambda qt_table_widget_set_item
    ((pointer void) int int UTF-8-string) void
    "qt_table_widget_set_item")
  (define-c-lambda qt_table_widget_item_text
    ((pointer void) int int) UTF-8-string
    "ffi_qt_table_widget_item_text")
  (define-c-lambda qt_table_widget_set_horizontal_header_item
    ((pointer void) int UTF-8-string) void
    "qt_table_widget_set_horizontal_header_item")
  (define-c-lambda qt_table_widget_set_vertical_header_item
    ((pointer void) int UTF-8-string) void
    "qt_table_widget_set_vertical_header_item")
  (define-c-lambda qt_table_widget_set_row_count ((pointer void) int) void
    "qt_table_widget_set_row_count")
  (define-c-lambda qt_table_widget_set_column_count ((pointer void) int) void
    "qt_table_widget_set_column_count")
  (define-c-lambda qt_table_widget_row_count ((pointer void)) int
    "qt_table_widget_row_count")
  (define-c-lambda qt_table_widget_column_count ((pointer void)) int
    "qt_table_widget_column_count")
  (define-c-lambda qt_table_widget_current_row ((pointer void)) int
    "qt_table_widget_current_row")
  (define-c-lambda qt_table_widget_current_column ((pointer void)) int
    "qt_table_widget_current_column")
  (define-c-lambda qt_table_widget_clear ((pointer void)) void
    "qt_table_widget_clear")
  (define-c-lambda raw_qt_table_widget_on_cell_clicked
    ((pointer void) long) void
    "ffi_qt_table_widget_on_cell_clicked")

  ;; ---- Tab Widget ----
  (define-c-lambda qt_tab_widget_create ((pointer void)) (pointer void)
    "qt_tab_widget_create")
  (define-c-lambda qt_tab_widget_add_tab
    ((pointer void) (pointer void) UTF-8-string) int
    "qt_tab_widget_add_tab")
  (define-c-lambda qt_tab_widget_set_current_index ((pointer void) int) void
    "qt_tab_widget_set_current_index")
  (define-c-lambda qt_tab_widget_current_index ((pointer void)) int
    "qt_tab_widget_current_index")
  (define-c-lambda qt_tab_widget_count ((pointer void)) int
    "qt_tab_widget_count")
  (define-c-lambda qt_tab_widget_set_tab_text
    ((pointer void) int UTF-8-string) void
    "qt_tab_widget_set_tab_text")
  (define-c-lambda raw_qt_tab_widget_on_current_changed
    ((pointer void) long) void
    "ffi_qt_tab_widget_on_current_changed")

  ;; ---- Progress Bar ----
  (define-c-lambda qt_progress_bar_create ((pointer void)) (pointer void)
    "qt_progress_bar_create")
  (define-c-lambda qt_progress_bar_set_value ((pointer void) int) void
    "qt_progress_bar_set_value")
  (define-c-lambda qt_progress_bar_value ((pointer void)) int
    "qt_progress_bar_value")
  (define-c-lambda qt_progress_bar_set_range ((pointer void) int int) void
    "qt_progress_bar_set_range")
  (define-c-lambda qt_progress_bar_set_format ((pointer void) UTF-8-string) void
    "qt_progress_bar_set_format")

  ;; ---- Slider ----
  (define-c-lambda qt_slider_create (int (pointer void)) (pointer void)
    "qt_slider_create")
  (define-c-lambda qt_slider_set_value ((pointer void) int) void
    "qt_slider_set_value")
  (define-c-lambda qt_slider_value ((pointer void)) int
    "qt_slider_value")
  (define-c-lambda qt_slider_set_range ((pointer void) int int) void
    "qt_slider_set_range")
  (define-c-lambda qt_slider_set_single_step ((pointer void) int) void
    "qt_slider_set_single_step")
  (define-c-lambda qt_slider_set_tick_interval ((pointer void) int) void
    "qt_slider_set_tick_interval")
  (define-c-lambda qt_slider_set_tick_position ((pointer void) int) void
    "qt_slider_set_tick_position")
  (define-c-lambda raw_qt_slider_on_value_changed ((pointer void) long) void
    "ffi_qt_slider_on_value_changed")

  ;; ---- Grid Layout ----
  (define-c-lambda qt_grid_layout_create ((pointer void)) (pointer void)
    "qt_grid_layout_create")
  (define-c-lambda qt_grid_layout_add_widget
    ((pointer void) (pointer void) int int int int) void
    "qt_grid_layout_add_widget")
  (define-c-lambda qt_grid_layout_set_row_stretch ((pointer void) int int) void
    "qt_grid_layout_set_row_stretch")
  (define-c-lambda qt_grid_layout_set_column_stretch ((pointer void) int int) void
    "qt_grid_layout_set_column_stretch")
  (define-c-lambda qt_grid_layout_set_row_minimum_height ((pointer void) int int) void
    "qt_grid_layout_set_row_minimum_height")
  (define-c-lambda qt_grid_layout_set_column_minimum_width ((pointer void) int int) void
    "qt_grid_layout_set_column_minimum_width")

  ;; ---- Timer ----
  (define-c-lambda qt_timer_create () (pointer void)
    "qt_timer_create")
  (define-c-lambda qt_timer_start ((pointer void) int) void
    "qt_timer_start")
  (define-c-lambda qt_timer_stop ((pointer void)) void
    "qt_timer_stop")
  (define-c-lambda qt_timer_set_single_shot ((pointer void) int) void
    "qt_timer_set_single_shot")
  (define-c-lambda qt_timer_is_active ((pointer void)) int
    "qt_timer_is_active")
  (define-c-lambda qt_timer_interval ((pointer void)) int
    "qt_timer_interval")
  (define-c-lambda qt_timer_set_interval ((pointer void) int) void
    "qt_timer_set_interval")
  (define-c-lambda raw_qt_timer_on_timeout ((pointer void) long) void
    "ffi_qt_timer_on_timeout")
  (define-c-lambda raw_qt_timer_single_shot (int long) void
    "ffi_qt_timer_single_shot")
  (define-c-lambda qt_timer_destroy ((pointer void)) void
    "qt_timer_destroy")

  ;; ---- Clipboard ----
  (define-c-lambda qt_clipboard_text ((pointer void)) UTF-8-string
    "ffi_qt_clipboard_text")
  (define-c-lambda qt_clipboard_set_text ((pointer void) UTF-8-string) void
    "qt_clipboard_set_text")
  (define-c-lambda raw_qt_clipboard_on_changed ((pointer void) long) void
    "ffi_qt_clipboard_on_changed")

  ;; ---- Tree Widget ----
  (define-c-lambda qt_tree_widget_create ((pointer void)) (pointer void)
    "qt_tree_widget_create")
  (define-c-lambda qt_tree_widget_set_column_count ((pointer void) int) void
    "qt_tree_widget_set_column_count")
  (define-c-lambda qt_tree_widget_column_count ((pointer void)) int
    "qt_tree_widget_column_count")
  (define-c-lambda qt_tree_widget_set_header_label ((pointer void) UTF-8-string) void
    "qt_tree_widget_set_header_label")
  (define-c-lambda qt_tree_widget_set_header_item_text
    ((pointer void) int UTF-8-string) void
    "qt_tree_widget_set_header_item_text")
  (define-c-lambda qt_tree_widget_add_top_level_item
    ((pointer void) (pointer void)) void
    "qt_tree_widget_add_top_level_item")
  (define-c-lambda qt_tree_widget_top_level_item_count ((pointer void)) int
    "qt_tree_widget_top_level_item_count")
  (define-c-lambda qt_tree_widget_top_level_item ((pointer void) int) (pointer void)
    "qt_tree_widget_top_level_item")
  (define-c-lambda qt_tree_widget_current_item ((pointer void)) (pointer void)
    "qt_tree_widget_current_item")
  (define-c-lambda qt_tree_widget_set_current_item
    ((pointer void) (pointer void)) void
    "qt_tree_widget_set_current_item")
  (define-c-lambda qt_tree_widget_expand_item
    ((pointer void) (pointer void)) void
    "qt_tree_widget_expand_item")
  (define-c-lambda qt_tree_widget_collapse_item
    ((pointer void) (pointer void)) void
    "qt_tree_widget_collapse_item")
  (define-c-lambda qt_tree_widget_expand_all ((pointer void)) void
    "qt_tree_widget_expand_all")
  (define-c-lambda qt_tree_widget_collapse_all ((pointer void)) void
    "qt_tree_widget_collapse_all")
  (define-c-lambda qt_tree_widget_clear ((pointer void)) void
    "qt_tree_widget_clear")
  (define-c-lambda raw_qt_tree_widget_on_current_item_changed
    ((pointer void) long) void
    "ffi_qt_tree_widget_on_current_item_changed")
  (define-c-lambda raw_qt_tree_widget_on_item_double_clicked
    ((pointer void) long) void
    "ffi_qt_tree_widget_on_item_double_clicked")
  (define-c-lambda raw_qt_tree_widget_on_item_expanded
    ((pointer void) long) void
    "ffi_qt_tree_widget_on_item_expanded")
  (define-c-lambda raw_qt_tree_widget_on_item_collapsed
    ((pointer void) long) void
    "ffi_qt_tree_widget_on_item_collapsed")

  ;; ---- Tree Widget Item ----
  (define-c-lambda qt_tree_item_create (UTF-8-string) (pointer void)
    "qt_tree_item_create")
  (define-c-lambda qt_tree_item_set_text ((pointer void) int UTF-8-string) void
    "qt_tree_item_set_text")
  (define-c-lambda qt_tree_item_text ((pointer void) int) UTF-8-string
    "ffi_qt_tree_item_text")
  (define-c-lambda qt_tree_item_add_child ((pointer void) (pointer void)) void
    "qt_tree_item_add_child")
  (define-c-lambda qt_tree_item_child_count ((pointer void)) int
    "qt_tree_item_child_count")
  (define-c-lambda qt_tree_item_child ((pointer void) int) (pointer void)
    "qt_tree_item_child")
  (define-c-lambda qt_tree_item_parent ((pointer void)) (pointer void)
    "qt_tree_item_parent")
  (define-c-lambda qt_tree_item_set_expanded ((pointer void) int) void
    "qt_tree_item_set_expanded")
  (define-c-lambda qt_tree_item_is_expanded ((pointer void)) int
    "qt_tree_item_is_expanded")

  ;; ---- App-wide Style Sheet ----
  (define-c-lambda qt_application_set_style_sheet
    ((pointer void) UTF-8-string) void
    "qt_application_set_style_sheet")

  ;; ---- Window State Management ----
  (define-c-lambda qt_widget_show_minimized ((pointer void)) void
    "qt_widget_show_minimized")
  (define-c-lambda qt_widget_show_maximized ((pointer void)) void
    "qt_widget_show_maximized")
  (define-c-lambda qt_widget_show_fullscreen ((pointer void)) void
    "qt_widget_show_fullscreen")
  (define-c-lambda qt_widget_show_normal ((pointer void)) void
    "qt_widget_show_normal")
  (define-c-lambda qt_widget_window_state ((pointer void)) int
    "qt_widget_window_state")
  (define-c-lambda qt_widget_move ((pointer void) int int) void
    "qt_widget_move")
  (define-c-lambda qt_widget_x ((pointer void)) int
    "qt_widget_x")
  (define-c-lambda qt_widget_y ((pointer void)) int
    "qt_widget_y")
  (define-c-lambda qt_widget_width ((pointer void)) int
    "qt_widget_width")
  (define-c-lambda qt_widget_height ((pointer void)) int
    "qt_widget_height")

  ;; ---- Scroll Area ----
  (define-c-lambda qt_scroll_area_create ((pointer void)) (pointer void)
    "qt_scroll_area_create")
  (define-c-lambda qt_scroll_area_set_widget
    ((pointer void) (pointer void)) void
    "qt_scroll_area_set_widget")
  (define-c-lambda qt_scroll_area_set_widget_resizable
    ((pointer void) int) void
    "qt_scroll_area_set_widget_resizable")
  (define-c-lambda qt_scroll_area_set_horizontal_scrollbar_policy
    ((pointer void) int) void
    "qt_scroll_area_set_horizontal_scrollbar_policy")
  (define-c-lambda qt_scroll_area_set_vertical_scrollbar_policy
    ((pointer void) int) void
    "qt_scroll_area_set_vertical_scrollbar_policy")

  ;; ---- Splitter ----
  (define-c-lambda qt_splitter_create (int (pointer void)) (pointer void)
    "qt_splitter_create")
  (define-c-lambda qt_splitter_add_widget ((pointer void) (pointer void)) void
    "qt_splitter_add_widget")
  (define-c-lambda qt_splitter_count ((pointer void)) int
    "qt_splitter_count")
  (define-c-lambda qt_splitter_set_sizes_2 ((pointer void) int int) void
    "qt_splitter_set_sizes_2")
  (define-c-lambda qt_splitter_set_sizes_3 ((pointer void) int int int) void
    "qt_splitter_set_sizes_3")
  (define-c-lambda qt_splitter_size_at ((pointer void) int) int
    "qt_splitter_size_at")
  (define-c-lambda qt_splitter_set_stretch_factor ((pointer void) int int) void
    "qt_splitter_set_stretch_factor")
  (define-c-lambda qt_splitter_set_handle_width ((pointer void) int) void
    "qt_splitter_set_handle_width")
  (define-c-lambda qt_splitter_set_collapsible ((pointer void) int int) void
    "qt_splitter_set_collapsible")
  (define-c-lambda qt_splitter_is_collapsible ((pointer void) int) int
    "qt_splitter_is_collapsible")

  ;; ---- Keyboard Events ----
  (define-c-lambda raw_qt_widget_install_key_handler
    ((pointer void) long) void
    "ffi_qt_widget_install_key_handler")
  (define-c-lambda qt_last_key_code () int
    "qt_last_key_code")
  (define-c-lambda qt_last_key_modifiers () int
    "qt_last_key_modifiers")
  (define-c-lambda qt_last_key_text () UTF-8-string
    "ffi_qt_last_key_text")

  ;; ---- Pixmap ----
  (define-c-lambda qt_pixmap_load (UTF-8-string) (pointer void)
    "qt_pixmap_load")
  (define-c-lambda qt_pixmap_width ((pointer void)) int
    "qt_pixmap_width")
  (define-c-lambda qt_pixmap_height ((pointer void)) int
    "qt_pixmap_height")
  (define-c-lambda qt_pixmap_is_null ((pointer void)) int
    "qt_pixmap_is_null")
  (define-c-lambda qt_pixmap_scaled ((pointer void) int int) (pointer void)
    "qt_pixmap_scaled")
  (define-c-lambda qt_pixmap_destroy ((pointer void)) void
    "qt_pixmap_destroy")
  (define-c-lambda qt_label_set_pixmap ((pointer void) (pointer void)) void
    "qt_label_set_pixmap")

  ;; ---- Icon ----
  (define-c-lambda qt_icon_create (UTF-8-string) (pointer void)
    "qt_icon_create")
  (define-c-lambda qt_icon_create_from_pixmap ((pointer void)) (pointer void)
    "qt_icon_create_from_pixmap")
  (define-c-lambda qt_icon_is_null ((pointer void)) int
    "qt_icon_is_null")
  (define-c-lambda qt_icon_destroy ((pointer void)) void
    "qt_icon_destroy")
  (define-c-lambda qt_push_button_set_icon ((pointer void) (pointer void)) void
    "qt_push_button_set_icon")
  (define-c-lambda qt_action_set_icon ((pointer void) (pointer void)) void
    "qt_action_set_icon")
  (define-c-lambda qt_widget_set_window_icon ((pointer void) (pointer void)) void
    "qt_widget_set_window_icon")

  ;; ---- Radio Button ----
  (define-c-lambda qt_radio_button_create
    (UTF-8-string (pointer void)) (pointer void)
    "qt_radio_button_create")
  (define-c-lambda qt_radio_button_set_text ((pointer void) UTF-8-string) void
    "qt_radio_button_set_text")
  (define-c-lambda qt_radio_button_text ((pointer void)) UTF-8-string
    "ffi_qt_radio_button_text")
  (define-c-lambda qt_radio_button_set_checked ((pointer void) int) void
    "qt_radio_button_set_checked")
  (define-c-lambda qt_radio_button_is_checked ((pointer void)) int
    "qt_radio_button_is_checked")
  (define-c-lambda raw_qt_radio_button_on_toggled ((pointer void) long) void
    "ffi_qt_radio_button_on_toggled")

  ;; ---- Button Group ----
  (define-c-lambda qt_button_group_create () (pointer void)
    "qt_button_group_create")
  (define-c-lambda qt_button_group_add_button
    ((pointer void) (pointer void) int) void
    "qt_button_group_add_button")
  (define-c-lambda qt_button_group_remove_button
    ((pointer void) (pointer void)) void
    "qt_button_group_remove_button")
  (define-c-lambda qt_button_group_checked_id ((pointer void)) int
    "qt_button_group_checked_id")
  (define-c-lambda qt_button_group_set_exclusive ((pointer void) int) void
    "qt_button_group_set_exclusive")
  (define-c-lambda qt_button_group_is_exclusive ((pointer void)) int
    "qt_button_group_is_exclusive")
  (define-c-lambda raw_qt_button_group_on_id_clicked ((pointer void) long) void
    "ffi_qt_button_group_on_id_clicked")
  (define-c-lambda qt_button_group_destroy ((pointer void)) void
    "qt_button_group_destroy")

  ;; ---- Group Box ----
  (define-c-lambda qt_group_box_create
    (UTF-8-string (pointer void)) (pointer void)
    "qt_group_box_create")
  (define-c-lambda qt_group_box_set_title ((pointer void) UTF-8-string) void
    "qt_group_box_set_title")
  (define-c-lambda qt_group_box_title ((pointer void)) UTF-8-string
    "ffi_qt_group_box_title")
  (define-c-lambda qt_group_box_set_checkable ((pointer void) int) void
    "qt_group_box_set_checkable")
  (define-c-lambda qt_group_box_is_checkable ((pointer void)) int
    "qt_group_box_is_checkable")
  (define-c-lambda qt_group_box_set_checked ((pointer void) int) void
    "qt_group_box_set_checked")
  (define-c-lambda qt_group_box_is_checked ((pointer void)) int
    "qt_group_box_is_checked")
  (define-c-lambda raw_qt_group_box_on_toggled ((pointer void) long) void
    "ffi_qt_group_box_on_toggled")

  ;; ---- Font ----
  (define-c-lambda qt_font_create (UTF-8-string int) (pointer void)
    "qt_font_create")
  (define-c-lambda qt_font_family ((pointer void)) UTF-8-string
    "ffi_qt_font_family")
  (define-c-lambda qt_font_point_size ((pointer void)) int
    "qt_font_point_size")
  (define-c-lambda qt_font_set_bold ((pointer void) int) void
    "qt_font_set_bold")
  (define-c-lambda qt_font_is_bold ((pointer void)) int
    "qt_font_is_bold")
  (define-c-lambda qt_font_set_italic ((pointer void) int) void
    "qt_font_set_italic")
  (define-c-lambda qt_font_is_italic ((pointer void)) int
    "qt_font_is_italic")
  (define-c-lambda qt_font_destroy ((pointer void)) void
    "qt_font_destroy")
  (define-c-lambda qt_widget_set_font ((pointer void) (pointer void)) void
    "qt_widget_set_font")
  (define-c-lambda qt_widget_font ((pointer void)) (pointer void)
    "qt_widget_font")

  ;; ---- Color ----
  (define-c-lambda qt_color_create_rgb (int int int int) (pointer void)
    "qt_color_create_rgb")
  (define-c-lambda qt_color_create_name (UTF-8-string) (pointer void)
    "qt_color_create_name")
  (define-c-lambda qt_color_red ((pointer void)) int
    "qt_color_red")
  (define-c-lambda qt_color_green ((pointer void)) int
    "qt_color_green")
  (define-c-lambda qt_color_blue ((pointer void)) int
    "qt_color_blue")
  (define-c-lambda qt_color_alpha ((pointer void)) int
    "qt_color_alpha")
  (define-c-lambda qt_color_name ((pointer void)) UTF-8-string
    "ffi_qt_color_name")
  (define-c-lambda qt_color_is_valid ((pointer void)) int
    "qt_color_is_valid")
  (define-c-lambda qt_color_destroy ((pointer void)) void
    "qt_color_destroy")

  ;; ---- Font Dialog ----
  (define-c-lambda qt_font_dialog_get_font ((pointer void)) (pointer void)
    "qt_font_dialog_get_font")

  ;; ---- Color Dialog ----
  (define-c-lambda qt_color_dialog_get_color
    (UTF-8-string (pointer void)) (pointer void)
    "qt_color_dialog_get_color")

  ;; ---- Stacked Widget ----
  (define-c-lambda qt_stacked_widget_create ((pointer void)) (pointer void)
    "qt_stacked_widget_create")
  (define-c-lambda qt_stacked_widget_add_widget
    ((pointer void) (pointer void)) int
    "qt_stacked_widget_add_widget")
  (define-c-lambda qt_stacked_widget_set_current_index ((pointer void) int) void
    "qt_stacked_widget_set_current_index")
  (define-c-lambda qt_stacked_widget_current_index ((pointer void)) int
    "qt_stacked_widget_current_index")
  (define-c-lambda qt_stacked_widget_count ((pointer void)) int
    "qt_stacked_widget_count")
  (define-c-lambda raw_qt_stacked_widget_on_current_changed
    ((pointer void) long) void
    "ffi_qt_stacked_widget_on_current_changed")

  ;; ---- Dock Widget ----
  (define-c-lambda qt_dock_widget_create
    (UTF-8-string (pointer void)) (pointer void)
    "qt_dock_widget_create")
  (define-c-lambda qt_dock_widget_set_widget
    ((pointer void) (pointer void)) void
    "qt_dock_widget_set_widget")
  (define-c-lambda qt_dock_widget_widget ((pointer void)) (pointer void)
    "qt_dock_widget_widget")
  (define-c-lambda qt_dock_widget_set_title ((pointer void) UTF-8-string) void
    "qt_dock_widget_set_title")
  (define-c-lambda qt_dock_widget_title ((pointer void)) UTF-8-string
    "ffi_qt_dock_widget_title")
  (define-c-lambda qt_dock_widget_set_floating ((pointer void) int) void
    "qt_dock_widget_set_floating")
  (define-c-lambda qt_dock_widget_is_floating ((pointer void)) int
    "qt_dock_widget_is_floating")
  (define-c-lambda qt_main_window_add_dock_widget
    ((pointer void) int (pointer void)) void
    "qt_main_window_add_dock_widget")

  ;; ---- System Tray Icon ----
  (define-c-lambda qt_system_tray_icon_create
    ((pointer void) (pointer void)) (pointer void)
    "qt_system_tray_icon_create")
  (define-c-lambda qt_system_tray_icon_set_tooltip
    ((pointer void) UTF-8-string) void
    "qt_system_tray_icon_set_tooltip")
  (define-c-lambda qt_system_tray_icon_set_icon
    ((pointer void) (pointer void)) void
    "qt_system_tray_icon_set_icon")
  (define-c-lambda qt_system_tray_icon_show ((pointer void)) void
    "qt_system_tray_icon_show")
  (define-c-lambda qt_system_tray_icon_hide ((pointer void)) void
    "qt_system_tray_icon_hide")
  (define-c-lambda qt_system_tray_icon_show_message
    ((pointer void) UTF-8-string UTF-8-string int int) void
    "qt_system_tray_icon_show_message")
  (define-c-lambda qt_system_tray_icon_set_context_menu
    ((pointer void) (pointer void)) void
    "qt_system_tray_icon_set_context_menu")
  (define-c-lambda raw_qt_system_tray_icon_on_activated
    ((pointer void) long) void
    "ffi_qt_system_tray_icon_on_activated")
  (define-c-lambda qt_system_tray_icon_is_available () int
    "qt_system_tray_icon_is_available")
  (define-c-lambda qt_system_tray_icon_destroy ((pointer void)) void
    "qt_system_tray_icon_destroy")

  ;; ---- QPainter ----
  (define-c-lambda qt_pixmap_create_blank (int int) (pointer void)
    "qt_pixmap_create_blank")
  (define-c-lambda qt_pixmap_fill ((pointer void) int int int int) void
    "qt_pixmap_fill")
  (define-c-lambda qt_painter_create ((pointer void)) (pointer void)
    "qt_painter_create")
  (define-c-lambda qt_painter_end ((pointer void)) void
    "qt_painter_end")
  (define-c-lambda qt_painter_destroy ((pointer void)) void
    "qt_painter_destroy")
  (define-c-lambda qt_painter_set_pen_color
    ((pointer void) int int int int) void
    "qt_painter_set_pen_color")
  (define-c-lambda qt_painter_set_pen_width ((pointer void) int) void
    "qt_painter_set_pen_width")
  (define-c-lambda qt_painter_set_brush_color
    ((pointer void) int int int int) void
    "qt_painter_set_brush_color")
  (define-c-lambda qt_painter_set_font_painter
    ((pointer void) (pointer void)) void
    "qt_painter_set_font")
  (define-c-lambda qt_painter_set_antialiasing ((pointer void) int) void
    "qt_painter_set_antialiasing")
  (define-c-lambda qt_painter_draw_line
    ((pointer void) int int int int) void
    "qt_painter_draw_line")
  (define-c-lambda qt_painter_draw_rect
    ((pointer void) int int int int) void
    "qt_painter_draw_rect")
  (define-c-lambda qt_painter_fill_rect
    ((pointer void) int int int int int int int int) void
    "qt_painter_fill_rect")
  (define-c-lambda qt_painter_draw_ellipse
    ((pointer void) int int int int) void
    "qt_painter_draw_ellipse")
  (define-c-lambda qt_painter_draw_text
    ((pointer void) int int UTF-8-string) void
    "qt_painter_draw_text")
  (define-c-lambda qt_painter_draw_text_rect
    ((pointer void) int int int int int UTF-8-string) void
    "qt_painter_draw_text_rect")
  (define-c-lambda qt_painter_draw_pixmap
    ((pointer void) int int (pointer void)) void
    "qt_painter_draw_pixmap")
  (define-c-lambda qt_painter_draw_point ((pointer void) int int) void
    "qt_painter_draw_point")
  (define-c-lambda qt_painter_draw_arc
    ((pointer void) int int int int int int) void
    "qt_painter_draw_arc")
  (define-c-lambda qt_painter_save ((pointer void)) void
    "qt_painter_save")
  (define-c-lambda qt_painter_restore ((pointer void)) void
    "qt_painter_restore")
  (define-c-lambda qt_painter_translate ((pointer void) int int) void
    "qt_painter_translate")
  (define-c-lambda qt_painter_rotate ((pointer void) double) void
    "qt_painter_rotate")
  (define-c-lambda qt_painter_scale ((pointer void) double double) void
    "qt_painter_scale")

  ;; ---- Drag and Drop ----
  (define-c-lambda qt_widget_set_accept_drops ((pointer void) int) void
    "qt_widget_set_accept_drops")
  (define-c-lambda raw_qt_drop_filter_install ((pointer void) long) (pointer void)
    "ffi_qt_drop_filter_install")
  (define-c-lambda qt_drop_filter_last_text ((pointer void)) UTF-8-string
    "ffi_qt_drop_filter_last_text")
  (define-c-lambda qt_drop_filter_destroy ((pointer void)) void
    "qt_drop_filter_destroy")
  (define-c-lambda qt_drag_text ((pointer void) UTF-8-string) void
    "qt_drag_text")

  ;; ---- Frame constants ----
  (define-const QT_FRAME_NO_FRAME)
  (define-const QT_FRAME_BOX)
  (define-const QT_FRAME_PANEL)
  (define-const QT_FRAME_WIN_PANEL)
  (define-const QT_FRAME_HLINE)
  (define-const QT_FRAME_VLINE)
  (define-const QT_FRAME_STYLED_PANEL)
  (define-const QT_FRAME_PLAIN)
  (define-const QT_FRAME_RAISED)
  (define-const QT_FRAME_SUNKEN)

  ;; ---- Double Spin Box ----
  (define-c-lambda qt_double_spin_box_create ((pointer void)) (pointer void)
    "qt_double_spin_box_create")
  (define-c-lambda qt_double_spin_box_set_value ((pointer void) double) void
    "qt_double_spin_box_set_value")
  (define-c-lambda qt_double_spin_box_value ((pointer void)) double
    "qt_double_spin_box_value")
  (define-c-lambda qt_double_spin_box_set_range ((pointer void) double double) void
    "qt_double_spin_box_set_range")
  (define-c-lambda qt_double_spin_box_set_single_step ((pointer void) double) void
    "qt_double_spin_box_set_single_step")
  (define-c-lambda qt_double_spin_box_set_decimals ((pointer void) int) void
    "qt_double_spin_box_set_decimals")
  (define-c-lambda qt_double_spin_box_decimals ((pointer void)) int
    "qt_double_spin_box_decimals")
  (define-c-lambda qt_double_spin_box_set_prefix ((pointer void) UTF-8-string) void
    "qt_double_spin_box_set_prefix")
  (define-c-lambda qt_double_spin_box_set_suffix ((pointer void) UTF-8-string) void
    "qt_double_spin_box_set_suffix")
  (define-c-lambda raw_qt_double_spin_box_on_value_changed
    ((pointer void) long) void
    "ffi_qt_double_spin_box_on_value_changed")

  ;; ---- Date Edit ----
  (define-c-lambda qt_date_edit_create ((pointer void)) (pointer void)
    "qt_date_edit_create")
  (define-c-lambda qt_date_edit_set_date ((pointer void) int int int) void
    "qt_date_edit_set_date")
  (define-c-lambda qt_date_edit_year ((pointer void)) int
    "qt_date_edit_year")
  (define-c-lambda qt_date_edit_month ((pointer void)) int
    "qt_date_edit_month")
  (define-c-lambda qt_date_edit_day ((pointer void)) int
    "qt_date_edit_day")
  (define-c-lambda qt_date_edit_date_string ((pointer void)) UTF-8-string
    "ffi_qt_date_edit_date_string")
  (define-c-lambda qt_date_edit_set_minimum_date ((pointer void) int int int) void
    "qt_date_edit_set_minimum_date")
  (define-c-lambda qt_date_edit_set_maximum_date ((pointer void) int int int) void
    "qt_date_edit_set_maximum_date")
  (define-c-lambda qt_date_edit_set_calendar_popup ((pointer void) int) void
    "qt_date_edit_set_calendar_popup")
  (define-c-lambda qt_date_edit_set_display_format ((pointer void) UTF-8-string) void
    "qt_date_edit_set_display_format")
  (define-c-lambda raw_qt_date_edit_on_date_changed ((pointer void) long) void
    "ffi_qt_date_edit_on_date_changed")

  ;; ---- Time Edit ----
  (define-c-lambda qt_time_edit_create ((pointer void)) (pointer void)
    "qt_time_edit_create")
  (define-c-lambda qt_time_edit_set_time ((pointer void) int int int) void
    "qt_time_edit_set_time")
  (define-c-lambda qt_time_edit_hour ((pointer void)) int
    "qt_time_edit_hour")
  (define-c-lambda qt_time_edit_minute ((pointer void)) int
    "qt_time_edit_minute")
  (define-c-lambda qt_time_edit_second ((pointer void)) int
    "qt_time_edit_second")
  (define-c-lambda qt_time_edit_time_string ((pointer void)) UTF-8-string
    "ffi_qt_time_edit_time_string")
  (define-c-lambda qt_time_edit_set_display_format ((pointer void) UTF-8-string) void
    "qt_time_edit_set_display_format")
  (define-c-lambda raw_qt_time_edit_on_time_changed ((pointer void) long) void
    "ffi_qt_time_edit_on_time_changed")

  ;; ---- Frame ----
  (define-c-lambda qt_frame_create ((pointer void)) (pointer void)
    "qt_frame_create")
  (define-c-lambda qt_frame_set_frame_shape ((pointer void) int) void
    "qt_frame_set_frame_shape")
  (define-c-lambda qt_frame_frame_shape ((pointer void)) int
    "qt_frame_frame_shape")
  (define-c-lambda qt_frame_set_frame_shadow ((pointer void) int) void
    "qt_frame_set_frame_shadow")
  (define-c-lambda qt_frame_frame_shadow ((pointer void)) int
    "qt_frame_frame_shadow")
  (define-c-lambda qt_frame_set_line_width ((pointer void) int) void
    "qt_frame_set_line_width")
  (define-c-lambda qt_frame_line_width ((pointer void)) int
    "qt_frame_line_width")
  (define-c-lambda qt_frame_set_mid_line_width ((pointer void) int) void
    "qt_frame_set_mid_line_width")

  ;; ---- Progress Dialog ----
  (define-c-lambda qt_progress_dialog_create
    (UTF-8-string UTF-8-string int int (pointer void)) (pointer void)
    "qt_progress_dialog_create")
  (define-c-lambda qt_progress_dialog_set_value ((pointer void) int) void
    "qt_progress_dialog_set_value")
  (define-c-lambda qt_progress_dialog_value ((pointer void)) int
    "qt_progress_dialog_value")
  (define-c-lambda qt_progress_dialog_set_range ((pointer void) int int) void
    "qt_progress_dialog_set_range")
  (define-c-lambda qt_progress_dialog_set_label_text
    ((pointer void) UTF-8-string) void
    "qt_progress_dialog_set_label_text")
  (define-c-lambda qt_progress_dialog_was_canceled ((pointer void)) int
    "qt_progress_dialog_was_canceled")
  (define-c-lambda qt_progress_dialog_set_minimum_duration ((pointer void) int) void
    "qt_progress_dialog_set_minimum_duration")
  (define-c-lambda qt_progress_dialog_set_auto_close ((pointer void) int) void
    "qt_progress_dialog_set_auto_close")
  (define-c-lambda qt_progress_dialog_set_auto_reset ((pointer void) int) void
    "qt_progress_dialog_set_auto_reset")
  (define-c-lambda qt_progress_dialog_reset ((pointer void)) void
    "qt_progress_dialog_reset")
  (define-c-lambda raw_qt_progress_dialog_on_canceled ((pointer void) long) void
    "ffi_qt_progress_dialog_on_canceled")

  ;; ---- Input Dialog ----
  (define-c-lambda qt_input_dialog_get_text
    ((pointer void) UTF-8-string UTF-8-string UTF-8-string) UTF-8-string
    "ffi_qt_input_dialog_get_text")
  (define-c-lambda qt_input_dialog_get_int
    ((pointer void) UTF-8-string UTF-8-string int int int int) int
    "qt_input_dialog_get_int")
  (define-c-lambda qt_input_dialog_get_double
    ((pointer void) UTF-8-string UTF-8-string double double double int) double
    "qt_input_dialog_get_double")
  (define-c-lambda qt_input_dialog_get_item
    ((pointer void) UTF-8-string UTF-8-string UTF-8-string int int) UTF-8-string
    "ffi_qt_input_dialog_get_item")
  (define-c-lambda qt_input_dialog_was_accepted () int
    "qt_input_dialog_was_accepted")

  ;; ---- Callback dispatch tables ----
  (define *qt-void-handlers* (make-hash-table))
  (define *qt-string-handlers* (make-hash-table))
  (define *qt-int-handlers* (make-hash-table))
  (define *qt-bool-handlers* (make-hash-table))
  (define *qt-next-callback-id* 0)

  (define (register-qt-void-handler! handler)
    (let ((id *qt-next-callback-id*))
      (set! *qt-next-callback-id* (+ id 1))
      (hash-put! *qt-void-handlers* id handler)
      id))

  (define (register-qt-string-handler! handler)
    (let ((id *qt-next-callback-id*))
      (set! *qt-next-callback-id* (+ id 1))
      (hash-put! *qt-string-handlers* id handler)
      id))

  (define (register-qt-int-handler! handler)
    (let ((id *qt-next-callback-id*))
      (set! *qt-next-callback-id* (+ id 1))
      (hash-put! *qt-int-handlers* id handler)
      id))

  (define (register-qt-bool-handler! handler)
    (let ((id *qt-next-callback-id*))
      (set! *qt-next-callback-id* (+ id 1))
      (hash-put! *qt-bool-handlers* id handler)
      id))

  ;; ---- c-define trampolines (Scheme functions callable from C) ----
  (c-define (ffi_qt_callback_void callback-id)
            (long) void
            "ffi_qt_callback_void" ""
    (let ((handler (hash-ref *qt-void-handlers* callback-id #f)))
      (when handler (handler))))

  (c-define (ffi_qt_callback_string callback-id value)
            (long UTF-8-string) void
            "ffi_qt_callback_string" ""
    (let ((handler (hash-ref *qt-string-handlers* callback-id #f)))
      (when handler (handler value))))

  (c-define (ffi_qt_callback_int callback-id value)
            (long int) void
            "ffi_qt_callback_int" ""
    (let ((handler (hash-ref *qt-int-handlers* callback-id #f)))
      (when handler (handler value))))

  (c-define (ffi_qt_callback_bool callback-id value)
            (long int) void
            "ffi_qt_callback_bool" ""
    (let ((handler (hash-ref *qt-bool-handlers* callback-id #f)))
      (when handler (handler (not (= value 0))))))

) ;; end begin-ffi
