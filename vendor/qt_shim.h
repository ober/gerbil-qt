#ifndef QT_SHIM_H
#define QT_SHIM_H

#ifdef __cplusplus
extern "C" {
#endif

/* --- Opaque handle types --- */
typedef void* qt_application_t;
typedef void* qt_widget_t;
typedef void* qt_main_window_t;
typedef void* qt_layout_t;
typedef void* qt_label_t;
typedef void* qt_push_button_t;

/* --- Callback signatures --- */
typedef void (*qt_callback_void)(long callback_id);
typedef void (*qt_callback_string)(long callback_id, const char* value);
typedef void (*qt_callback_int)(long callback_id, int value);
typedef void (*qt_callback_bool)(long callback_id, int value);

/* --- Application lifecycle --- */
qt_application_t qt_application_create(int argc, char** argv);
int              qt_application_exec(qt_application_t app);
void             qt_application_quit(qt_application_t app);
void             qt_application_process_events(qt_application_t app);
void             qt_application_destroy(qt_application_t app);

/* --- Widget base (applies to all widget types) --- */
qt_widget_t qt_widget_create(qt_widget_t parent);
void qt_widget_show(qt_widget_t w);
void qt_widget_hide(qt_widget_t w);
void qt_widget_close(qt_widget_t w);
void qt_widget_set_enabled(qt_widget_t w, int enabled);
int  qt_widget_is_enabled(qt_widget_t w);
void qt_widget_set_visible(qt_widget_t w, int visible);
void qt_widget_set_fixed_size(qt_widget_t w, int width, int height);
void qt_widget_set_minimum_size(qt_widget_t w, int width, int height);
void qt_widget_set_maximum_size(qt_widget_t w, int width, int height);
void qt_widget_resize(qt_widget_t w, int width, int height);
void qt_widget_set_style_sheet(qt_widget_t w, const char* css);
void qt_widget_set_tooltip(qt_widget_t w, const char* text);
void qt_widget_set_font_size(qt_widget_t w, int size);
void qt_widget_destroy(qt_widget_t w);

/* --- Main Window --- */
qt_main_window_t qt_main_window_create(qt_widget_t parent);
void qt_main_window_set_title(qt_main_window_t w, const char* title);
void qt_main_window_set_central_widget(qt_main_window_t w, qt_widget_t child);

/* --- Layouts --- */
qt_layout_t qt_vbox_layout_create(qt_widget_t parent);
qt_layout_t qt_hbox_layout_create(qt_widget_t parent);
void qt_layout_add_widget(qt_layout_t layout, qt_widget_t widget);
void qt_layout_add_stretch(qt_layout_t layout, int stretch);
void qt_layout_set_spacing(qt_layout_t layout, int spacing);
void qt_layout_set_margins(qt_layout_t layout, int left, int top,
                           int right, int bottom);

/* --- Labels --- */
qt_label_t qt_label_create(const char* text, qt_widget_t parent);
void qt_label_set_text(qt_label_t l, const char* text);
const char* qt_label_text(qt_label_t l);
void qt_label_set_alignment(qt_label_t l, int alignment);

/* --- Push Button --- */
qt_push_button_t qt_push_button_create(const char* text, qt_widget_t parent);
void qt_push_button_set_text(qt_push_button_t b, const char* text);
const char* qt_push_button_text(qt_push_button_t b);
void qt_push_button_on_clicked(qt_push_button_t b,
                               qt_callback_void callback,
                               long callback_id);

/* ========== Phase 2 widgets ========== */

typedef void* qt_line_edit_t;
typedef void* qt_check_box_t;
typedef void* qt_combo_box_t;
typedef void* qt_text_edit_t;
typedef void* qt_spin_box_t;
typedef void* qt_dialog_t;

/* --- Line Edit --- */
qt_line_edit_t qt_line_edit_create(qt_widget_t parent);
void        qt_line_edit_set_text(qt_line_edit_t e, const char* text);
const char* qt_line_edit_text(qt_line_edit_t e);
void        qt_line_edit_set_placeholder(qt_line_edit_t e, const char* text);
void        qt_line_edit_set_read_only(qt_line_edit_t e, int read_only);
void        qt_line_edit_set_echo_mode(qt_line_edit_t e, int mode);
void        qt_line_edit_on_text_changed(qt_line_edit_t e,
                                         qt_callback_string callback,
                                         long callback_id);
void        qt_line_edit_on_return_pressed(qt_line_edit_t e,
                                           qt_callback_void callback,
                                           long callback_id);

/* --- Check Box --- */
qt_check_box_t qt_check_box_create(const char* text, qt_widget_t parent);
void qt_check_box_set_text(qt_check_box_t c, const char* text);
void qt_check_box_set_checked(qt_check_box_t c, int checked);
int  qt_check_box_is_checked(qt_check_box_t c);
void qt_check_box_on_toggled(qt_check_box_t c,
                              qt_callback_bool callback,
                              long callback_id);

/* --- Combo Box --- */
qt_combo_box_t qt_combo_box_create(qt_widget_t parent);
void        qt_combo_box_add_item(qt_combo_box_t c, const char* text);
void        qt_combo_box_set_current_index(qt_combo_box_t c, int index);
int         qt_combo_box_current_index(qt_combo_box_t c);
const char* qt_combo_box_current_text(qt_combo_box_t c);
int         qt_combo_box_count(qt_combo_box_t c);
void        qt_combo_box_clear(qt_combo_box_t c);
void        qt_combo_box_on_current_index_changed(qt_combo_box_t c,
                                                   qt_callback_int callback,
                                                   long callback_id);

/* --- Text Edit --- */
qt_text_edit_t qt_text_edit_create(qt_widget_t parent);
void        qt_text_edit_set_text(qt_text_edit_t e, const char* text);
const char* qt_text_edit_text(qt_text_edit_t e);
void        qt_text_edit_set_placeholder(qt_text_edit_t e, const char* text);
void        qt_text_edit_set_read_only(qt_text_edit_t e, int read_only);
void        qt_text_edit_append(qt_text_edit_t e, const char* text);
void        qt_text_edit_clear(qt_text_edit_t e);
void        qt_text_edit_on_text_changed(qt_text_edit_t e,
                                          qt_callback_void callback,
                                          long callback_id);

/* --- Spin Box --- */
qt_spin_box_t qt_spin_box_create(qt_widget_t parent);
void qt_spin_box_set_value(qt_spin_box_t s, int value);
int  qt_spin_box_value(qt_spin_box_t s);
void qt_spin_box_set_range(qt_spin_box_t s, int minimum, int maximum);
void qt_spin_box_set_single_step(qt_spin_box_t s, int step);
void qt_spin_box_set_prefix(qt_spin_box_t s, const char* prefix);
void qt_spin_box_set_suffix(qt_spin_box_t s, const char* suffix);
void qt_spin_box_on_value_changed(qt_spin_box_t s,
                                   qt_callback_int callback,
                                   long callback_id);

/* --- Dialog --- */
qt_dialog_t qt_dialog_create(qt_widget_t parent);
int  qt_dialog_exec(qt_dialog_t d);
void qt_dialog_accept(qt_dialog_t d);
void qt_dialog_reject(qt_dialog_t d);
void qt_dialog_set_title(qt_dialog_t d, const char* title);

/* --- Message Box (static convenience) --- */
int qt_message_box_information(qt_widget_t parent, const char* title, const char* text);
int qt_message_box_warning(qt_widget_t parent, const char* title, const char* text);
int qt_message_box_question(qt_widget_t parent, const char* title, const char* text);
int qt_message_box_critical(qt_widget_t parent, const char* title, const char* text);

/* --- File Dialog (static convenience) --- */
const char* qt_file_dialog_open_file(qt_widget_t parent, const char* caption,
                                     const char* dir, const char* filter);
const char* qt_file_dialog_save_file(qt_widget_t parent, const char* caption,
                                     const char* dir, const char* filter);
const char* qt_file_dialog_open_directory(qt_widget_t parent, const char* caption,
                                          const char* dir);

/* ========== Phase 3: Menus, Actions, Toolbars ========== */

typedef void* qt_menu_bar_t;
typedef void* qt_menu_t;
typedef void* qt_action_t;
typedef void* qt_toolbar_t;

/* --- Menu Bar --- */
qt_menu_bar_t qt_main_window_menu_bar(qt_main_window_t w);

/* --- Menu --- */
qt_menu_t  qt_menu_bar_add_menu(qt_menu_bar_t bar, const char* title);
qt_menu_t  qt_menu_add_menu(qt_menu_t menu, const char* title);
void       qt_menu_add_action(qt_menu_t menu, qt_action_t action);
void       qt_menu_add_separator(qt_menu_t menu);

/* --- Action --- */
qt_action_t qt_action_create(const char* text, qt_widget_t parent);
void        qt_action_set_text(qt_action_t a, const char* text);
const char* qt_action_text(qt_action_t a);
void        qt_action_set_shortcut(qt_action_t a, const char* shortcut);
void        qt_action_set_enabled(qt_action_t a, int enabled);
int         qt_action_is_enabled(qt_action_t a);
void        qt_action_set_checkable(qt_action_t a, int checkable);
int         qt_action_is_checkable(qt_action_t a);
void        qt_action_set_checked(qt_action_t a, int checked);
int         qt_action_is_checked(qt_action_t a);
void        qt_action_set_tooltip(qt_action_t a, const char* text);
void        qt_action_set_status_tip(qt_action_t a, const char* text);
void        qt_action_on_triggered(qt_action_t a,
                                   qt_callback_void callback,
                                   long callback_id);
void        qt_action_on_toggled(qt_action_t a,
                                 qt_callback_bool callback,
                                 long callback_id);

/* --- Toolbar --- */
qt_toolbar_t qt_toolbar_create(const char* title, qt_widget_t parent);
void         qt_main_window_add_toolbar(qt_main_window_t w, qt_toolbar_t tb);
void         qt_toolbar_add_action(qt_toolbar_t tb, qt_action_t action);
void         qt_toolbar_add_separator(qt_toolbar_t tb);
void         qt_toolbar_add_widget(qt_toolbar_t tb, qt_widget_t w);
void         qt_toolbar_set_movable(qt_toolbar_t tb, int movable);
void         qt_toolbar_set_icon_size(qt_toolbar_t tb, int width, int height);

/* --- Status Bar --- */
void         qt_main_window_set_status_bar_text(qt_main_window_t w, const char* text);

/* ========== Phase 5: Grid Layout, Timer, Clipboard, Tree ========== */

/* --- Grid Layout --- */
qt_layout_t qt_grid_layout_create(qt_widget_t parent);
void qt_grid_layout_add_widget(qt_layout_t layout, qt_widget_t widget,
                               int row, int col, int row_span, int col_span);
void qt_grid_layout_set_row_stretch(qt_layout_t layout, int row, int stretch);
void qt_grid_layout_set_column_stretch(qt_layout_t layout, int col, int stretch);
void qt_grid_layout_set_row_minimum_height(qt_layout_t layout, int row, int height);
void qt_grid_layout_set_column_minimum_width(qt_layout_t layout, int col, int width);

/* --- Timer --- */
typedef void* qt_timer_t;

qt_timer_t qt_timer_create(void);
void       qt_timer_start(qt_timer_t t, int msec);
void       qt_timer_stop(qt_timer_t t);
void       qt_timer_set_single_shot(qt_timer_t t, int single_shot);
int        qt_timer_is_active(qt_timer_t t);
int        qt_timer_interval(qt_timer_t t);
void       qt_timer_set_interval(qt_timer_t t, int msec);
void       qt_timer_on_timeout(qt_timer_t t,
                               qt_callback_void callback,
                               long callback_id);
void       qt_timer_single_shot(int msec,
                                qt_callback_void callback,
                                long callback_id);
void       qt_timer_destroy(qt_timer_t t);

/* --- Clipboard --- */
const char* qt_clipboard_text(qt_application_t app);
void        qt_clipboard_set_text(qt_application_t app, const char* text);
void        qt_clipboard_on_changed(qt_application_t app,
                                    qt_callback_void callback,
                                    long callback_id);

/* --- Tree Widget --- */
typedef void* qt_tree_widget_t;
typedef void* qt_tree_item_t;

qt_tree_widget_t qt_tree_widget_create(qt_widget_t parent);
void        qt_tree_widget_set_column_count(qt_tree_widget_t t, int count);
int         qt_tree_widget_column_count(qt_tree_widget_t t);
void        qt_tree_widget_set_header_label(qt_tree_widget_t t, const char* label);
void        qt_tree_widget_set_header_item_text(qt_tree_widget_t t,
                                                 int col, const char* text);
void        qt_tree_widget_add_top_level_item(qt_tree_widget_t t, qt_tree_item_t item);
int         qt_tree_widget_top_level_item_count(qt_tree_widget_t t);
qt_tree_item_t qt_tree_widget_top_level_item(qt_tree_widget_t t, int index);
qt_tree_item_t qt_tree_widget_current_item(qt_tree_widget_t t);
void        qt_tree_widget_set_current_item(qt_tree_widget_t t, qt_tree_item_t item);
void        qt_tree_widget_expand_item(qt_tree_widget_t t, qt_tree_item_t item);
void        qt_tree_widget_collapse_item(qt_tree_widget_t t, qt_tree_item_t item);
void        qt_tree_widget_expand_all(qt_tree_widget_t t);
void        qt_tree_widget_collapse_all(qt_tree_widget_t t);
void        qt_tree_widget_clear(qt_tree_widget_t t);
void        qt_tree_widget_on_current_item_changed(qt_tree_widget_t t,
                                                    qt_callback_void callback,
                                                    long callback_id);
void        qt_tree_widget_on_item_double_clicked(qt_tree_widget_t t,
                                                   qt_callback_void callback,
                                                   long callback_id);
void        qt_tree_widget_on_item_expanded(qt_tree_widget_t t,
                                             qt_callback_void callback,
                                             long callback_id);
void        qt_tree_widget_on_item_collapsed(qt_tree_widget_t t,
                                              qt_callback_void callback,
                                              long callback_id);

/* --- Tree Widget Item --- */
qt_tree_item_t qt_tree_item_create(const char* text);
void        qt_tree_item_set_text(qt_tree_item_t item, int col, const char* text);
const char* qt_tree_item_text(qt_tree_item_t item, int col);
void        qt_tree_item_add_child(qt_tree_item_t parent, qt_tree_item_t child);
int         qt_tree_item_child_count(qt_tree_item_t item);
qt_tree_item_t qt_tree_item_child(qt_tree_item_t item, int index);
qt_tree_item_t qt_tree_item_parent(qt_tree_item_t item);
void        qt_tree_item_set_expanded(qt_tree_item_t item, int expanded);
int         qt_tree_item_is_expanded(qt_tree_item_t item);

/* ========== Phase 6: Style Sheets, Window State, ScrollArea, Splitter, Keys ========== */

typedef void* qt_scroll_area_t;
typedef void* qt_splitter_t;

/* --- App-wide Style Sheet --- */
void qt_application_set_style_sheet(qt_application_t app, const char* css);

/* --- Window State Management --- */
void qt_widget_show_minimized(qt_widget_t w);
void qt_widget_show_maximized(qt_widget_t w);
void qt_widget_show_fullscreen(qt_widget_t w);
void qt_widget_show_normal(qt_widget_t w);
int  qt_widget_window_state(qt_widget_t w);
void qt_widget_move(qt_widget_t w, int x, int y);
int  qt_widget_x(qt_widget_t w);
int  qt_widget_y(qt_widget_t w);
int  qt_widget_width(qt_widget_t w);
int  qt_widget_height(qt_widget_t w);

/* --- Scroll Area --- */
qt_scroll_area_t qt_scroll_area_create(qt_widget_t parent);
void qt_scroll_area_set_widget(qt_scroll_area_t s, qt_widget_t w);
void qt_scroll_area_set_widget_resizable(qt_scroll_area_t s, int resizable);
void qt_scroll_area_set_horizontal_scrollbar_policy(qt_scroll_area_t s, int policy);
void qt_scroll_area_set_vertical_scrollbar_policy(qt_scroll_area_t s, int policy);

/* --- Splitter --- */
qt_splitter_t qt_splitter_create(int orientation, qt_widget_t parent);
void qt_splitter_add_widget(qt_splitter_t s, qt_widget_t w);
int  qt_splitter_count(qt_splitter_t s);
void qt_splitter_set_sizes_2(qt_splitter_t s, int a, int b);
void qt_splitter_set_sizes_3(qt_splitter_t s, int a, int b, int c);
int  qt_splitter_size_at(qt_splitter_t s, int index);
void qt_splitter_set_stretch_factor(qt_splitter_t s, int index, int stretch);
void qt_splitter_set_handle_width(qt_splitter_t s, int width);
void qt_splitter_set_collapsible(qt_splitter_t s, int index, int collapsible);
int  qt_splitter_is_collapsible(qt_splitter_t s, int index);

/* --- Keyboard Events --- */
void qt_widget_install_key_handler(qt_widget_t w,
                                   qt_callback_void callback,
                                   long callback_id);
int         qt_last_key_code(void);
int         qt_last_key_modifiers(void);
const char* qt_last_key_text(void);

/* ========== Phase 7: Images, Icons, Radio Buttons, GroupBox ========== */

typedef void* qt_pixmap_t;
typedef void* qt_icon_t;
typedef void* qt_radio_button_t;
typedef void* qt_button_group_t;
typedef void* qt_group_box_t;

/* --- Pixmap --- */
qt_pixmap_t qt_pixmap_load(const char* path);
int         qt_pixmap_width(qt_pixmap_t p);
int         qt_pixmap_height(qt_pixmap_t p);
int         qt_pixmap_is_null(qt_pixmap_t p);
qt_pixmap_t qt_pixmap_scaled(qt_pixmap_t p, int w, int h);
void        qt_pixmap_destroy(qt_pixmap_t p);
void        qt_label_set_pixmap(qt_label_t label, qt_pixmap_t pixmap);

/* --- Icon --- */
qt_icon_t   qt_icon_create(const char* path);
qt_icon_t   qt_icon_create_from_pixmap(qt_pixmap_t pixmap);
int         qt_icon_is_null(qt_icon_t icon);
void        qt_icon_destroy(qt_icon_t icon);
void        qt_push_button_set_icon(qt_push_button_t button, qt_icon_t icon);
void        qt_action_set_icon(qt_action_t action, qt_icon_t icon);
void        qt_widget_set_window_icon(qt_widget_t widget, qt_icon_t icon);

/* --- Radio Button --- */
qt_radio_button_t qt_radio_button_create(const char* text, qt_widget_t parent);
void        qt_radio_button_set_text(qt_radio_button_t r, const char* text);
const char* qt_radio_button_text(qt_radio_button_t r);
void        qt_radio_button_set_checked(qt_radio_button_t r, int checked);
int         qt_radio_button_is_checked(qt_radio_button_t r);
void        qt_radio_button_on_toggled(qt_radio_button_t r,
                                        qt_callback_bool callback,
                                        long callback_id);

/* --- Button Group --- */
qt_button_group_t qt_button_group_create(void);
void        qt_button_group_add_button(qt_button_group_t bg,
                                        qt_widget_t button, int id);
void        qt_button_group_remove_button(qt_button_group_t bg,
                                           qt_widget_t button);
int         qt_button_group_checked_id(qt_button_group_t bg);
void        qt_button_group_set_exclusive(qt_button_group_t bg, int exclusive);
int         qt_button_group_is_exclusive(qt_button_group_t bg);
void        qt_button_group_on_id_clicked(qt_button_group_t bg,
                                           qt_callback_int callback,
                                           long callback_id);
void        qt_button_group_destroy(qt_button_group_t bg);

/* --- Group Box --- */
qt_group_box_t qt_group_box_create(const char* title, qt_widget_t parent);
void        qt_group_box_set_title(qt_group_box_t gb, const char* title);
const char* qt_group_box_title(qt_group_box_t gb);
void        qt_group_box_set_checkable(qt_group_box_t gb, int checkable);
int         qt_group_box_is_checkable(qt_group_box_t gb);
void        qt_group_box_set_checked(qt_group_box_t gb, int checked);
int         qt_group_box_is_checked(qt_group_box_t gb);
void        qt_group_box_on_toggled(qt_group_box_t gb,
                                     qt_callback_bool callback,
                                     long callback_id);

/* ========== Phase 8: Fonts, Colors, Dialogs, Dock, Tray, Painter, DnD ========== */

typedef void* qt_font_t;
typedef void* qt_color_t;
typedef void* qt_stacked_widget_t;
typedef void* qt_dock_widget_t;
typedef void* qt_tray_icon_t;
typedef void* qt_painter_t;
typedef void* qt_drop_filter_t;

/* --- Font --- */
qt_font_t   qt_font_create(const char* family, int point_size);
const char* qt_font_family(qt_font_t f);
int         qt_font_point_size(qt_font_t f);
void        qt_font_set_bold(qt_font_t f, int bold);
int         qt_font_is_bold(qt_font_t f);
void        qt_font_set_italic(qt_font_t f, int italic);
int         qt_font_is_italic(qt_font_t f);
void        qt_font_destroy(qt_font_t f);
void        qt_widget_set_font(qt_widget_t w, qt_font_t f);
qt_font_t   qt_widget_font(qt_widget_t w);

/* --- Color --- */
qt_color_t  qt_color_create_rgb(int r, int g, int b, int a);
qt_color_t  qt_color_create_name(const char* name);
int         qt_color_red(qt_color_t c);
int         qt_color_green(qt_color_t c);
int         qt_color_blue(qt_color_t c);
int         qt_color_alpha(qt_color_t c);
const char* qt_color_name(qt_color_t c);
int         qt_color_is_valid(qt_color_t c);
void        qt_color_destroy(qt_color_t c);

/* --- Font Dialog --- */
qt_font_t   qt_font_dialog_get_font(qt_widget_t parent);

/* --- Color Dialog --- */
qt_color_t  qt_color_dialog_get_color(const char* initial, qt_widget_t parent);

/* --- Stacked Widget --- */
qt_stacked_widget_t qt_stacked_widget_create(qt_widget_t parent);
int         qt_stacked_widget_add_widget(qt_stacked_widget_t sw, qt_widget_t w);
void        qt_stacked_widget_set_current_index(qt_stacked_widget_t sw, int idx);
int         qt_stacked_widget_current_index(qt_stacked_widget_t sw);
int         qt_stacked_widget_count(qt_stacked_widget_t sw);
void        qt_stacked_widget_on_current_changed(qt_stacked_widget_t sw,
                                                  qt_callback_int callback,
                                                  long callback_id);

/* --- Dock Widget --- */
qt_dock_widget_t qt_dock_widget_create(const char* title, qt_widget_t parent);
void        qt_dock_widget_set_widget(qt_dock_widget_t dw, qt_widget_t w);
qt_widget_t qt_dock_widget_widget(qt_dock_widget_t dw);
void        qt_dock_widget_set_title(qt_dock_widget_t dw, const char* title);
const char* qt_dock_widget_title(qt_dock_widget_t dw);
void        qt_dock_widget_set_floating(qt_dock_widget_t dw, int floating);
int         qt_dock_widget_is_floating(qt_dock_widget_t dw);
void        qt_main_window_add_dock_widget(qt_main_window_t mw, int area,
                                            qt_dock_widget_t dw);

/* --- System Tray Icon --- */
qt_tray_icon_t qt_system_tray_icon_create(qt_icon_t icon, qt_widget_t parent);
void        qt_system_tray_icon_set_tooltip(qt_tray_icon_t ti, const char* text);
void        qt_system_tray_icon_set_icon(qt_tray_icon_t ti, qt_icon_t icon);
void        qt_system_tray_icon_show(qt_tray_icon_t ti);
void        qt_system_tray_icon_hide(qt_tray_icon_t ti);
void        qt_system_tray_icon_show_message(qt_tray_icon_t ti,
                                              const char* title, const char* msg,
                                              int icon_type, int msecs);
void        qt_system_tray_icon_set_context_menu(qt_tray_icon_t ti,
                                                  qt_menu_t menu);
void        qt_system_tray_icon_on_activated(qt_tray_icon_t ti,
                                              qt_callback_int callback,
                                              long callback_id);
int         qt_system_tray_icon_is_available(void);
void        qt_system_tray_icon_destroy(qt_tray_icon_t ti);

/* --- QPainter (paint onto QPixmap) --- */
qt_pixmap_t qt_pixmap_create_blank(int w, int h);
void        qt_pixmap_fill(qt_pixmap_t pm, int r, int g, int b, int a);
qt_painter_t qt_painter_create(qt_pixmap_t pixmap);
void        qt_painter_end(qt_painter_t p);
void        qt_painter_destroy(qt_painter_t p);
void        qt_painter_set_pen_color(qt_painter_t p, int r, int g, int b, int a);
void        qt_painter_set_pen_width(qt_painter_t p, int width);
void        qt_painter_set_brush_color(qt_painter_t p, int r, int g, int b, int a);
void        qt_painter_set_font(qt_painter_t p, qt_font_t font);
void        qt_painter_set_antialiasing(qt_painter_t p, int enabled);
void        qt_painter_draw_line(qt_painter_t p, int x1, int y1, int x2, int y2);
void        qt_painter_draw_rect(qt_painter_t p, int x, int y, int w, int h);
void        qt_painter_fill_rect(qt_painter_t p, int x, int y, int w, int h,
                                  int r, int g, int b, int a);
void        qt_painter_draw_ellipse(qt_painter_t p, int x, int y, int w, int h);
void        qt_painter_draw_text(qt_painter_t p, int x, int y, const char* text);
void        qt_painter_draw_text_rect(qt_painter_t p, int x, int y, int w, int h,
                                       int flags, const char* text);
void        qt_painter_draw_pixmap(qt_painter_t p, int x, int y,
                                    qt_pixmap_t pixmap);
void        qt_painter_draw_point(qt_painter_t p, int x, int y);
void        qt_painter_draw_arc(qt_painter_t p, int x, int y, int w, int h,
                                 int start_angle, int span_angle);
void        qt_painter_save(qt_painter_t p);
void        qt_painter_restore(qt_painter_t p);
void        qt_painter_translate(qt_painter_t p, int dx, int dy);
void        qt_painter_rotate(qt_painter_t p, double angle);
void        qt_painter_scale(qt_painter_t p, double sx, double sy);

/* --- Drag and Drop --- */
void        qt_widget_set_accept_drops(qt_widget_t w, int accept);
qt_drop_filter_t qt_drop_filter_install(qt_widget_t widget,
                                         qt_callback_string callback,
                                         long callback_id);
const char* qt_drop_filter_last_text(qt_drop_filter_t df);
void        qt_drop_filter_destroy(qt_drop_filter_t df);
void        qt_drag_text(qt_widget_t source, const char* text);

/* ========== Phase 9: Practical Widgets & Dialog Enhancements ========== */

typedef void* qt_double_spin_box_t;
typedef void* qt_date_edit_t;
typedef void* qt_time_edit_t;
typedef void* qt_frame_t;
typedef void* qt_progress_dialog_t;

/* Frame shape constants (QFrame::Shape) */
#define QT_FRAME_NO_FRAME     0
#define QT_FRAME_BOX          1
#define QT_FRAME_PANEL        2
#define QT_FRAME_WIN_PANEL    3
#define QT_FRAME_HLINE        4
#define QT_FRAME_VLINE        5
#define QT_FRAME_STYLED_PANEL 6

/* Frame shadow constants (QFrame::Shadow) */
#define QT_FRAME_PLAIN        0x0010
#define QT_FRAME_RAISED       0x0020
#define QT_FRAME_SUNKEN       0x0030

/* --- Double Spin Box --- */
qt_double_spin_box_t qt_double_spin_box_create(qt_widget_t parent);
void        qt_double_spin_box_set_value(qt_double_spin_box_t s, double value);
double      qt_double_spin_box_value(qt_double_spin_box_t s);
void        qt_double_spin_box_set_range(qt_double_spin_box_t s,
                                          double minimum, double maximum);
void        qt_double_spin_box_set_single_step(qt_double_spin_box_t s, double step);
void        qt_double_spin_box_set_decimals(qt_double_spin_box_t s, int decimals);
int         qt_double_spin_box_decimals(qt_double_spin_box_t s);
void        qt_double_spin_box_set_prefix(qt_double_spin_box_t s, const char* prefix);
void        qt_double_spin_box_set_suffix(qt_double_spin_box_t s, const char* suffix);
void        qt_double_spin_box_on_value_changed(qt_double_spin_box_t s,
                                                 qt_callback_string callback,
                                                 long callback_id);

/* --- Date Edit --- */
qt_date_edit_t qt_date_edit_create(qt_widget_t parent);
void        qt_date_edit_set_date(qt_date_edit_t d, int year, int month, int day);
int         qt_date_edit_year(qt_date_edit_t d);
int         qt_date_edit_month(qt_date_edit_t d);
int         qt_date_edit_day(qt_date_edit_t d);
const char* qt_date_edit_date_string(qt_date_edit_t d);
void        qt_date_edit_set_minimum_date(qt_date_edit_t d,
                                           int year, int month, int day);
void        qt_date_edit_set_maximum_date(qt_date_edit_t d,
                                           int year, int month, int day);
void        qt_date_edit_set_calendar_popup(qt_date_edit_t d, int enabled);
void        qt_date_edit_set_display_format(qt_date_edit_t d, const char* format);
void        qt_date_edit_on_date_changed(qt_date_edit_t d,
                                          qt_callback_string callback,
                                          long callback_id);

/* --- Time Edit --- */
qt_time_edit_t qt_time_edit_create(qt_widget_t parent);
void        qt_time_edit_set_time(qt_time_edit_t t, int hour, int minute, int second);
int         qt_time_edit_hour(qt_time_edit_t t);
int         qt_time_edit_minute(qt_time_edit_t t);
int         qt_time_edit_second(qt_time_edit_t t);
const char* qt_time_edit_time_string(qt_time_edit_t t);
void        qt_time_edit_set_display_format(qt_time_edit_t t, const char* format);
void        qt_time_edit_on_time_changed(qt_time_edit_t t,
                                          qt_callback_string callback,
                                          long callback_id);

/* --- Frame --- */
qt_frame_t  qt_frame_create(qt_widget_t parent);
void        qt_frame_set_frame_shape(qt_frame_t f, int shape);
int         qt_frame_frame_shape(qt_frame_t f);
void        qt_frame_set_frame_shadow(qt_frame_t f, int shadow);
int         qt_frame_frame_shadow(qt_frame_t f);
void        qt_frame_set_line_width(qt_frame_t f, int width);
int         qt_frame_line_width(qt_frame_t f);
void        qt_frame_set_mid_line_width(qt_frame_t f, int width);

/* --- Progress Dialog --- */
qt_progress_dialog_t qt_progress_dialog_create(const char* label,
                                                const char* cancel_text,
                                                int minimum, int maximum,
                                                qt_widget_t parent);
void        qt_progress_dialog_set_value(qt_progress_dialog_t pd, int value);
int         qt_progress_dialog_value(qt_progress_dialog_t pd);
void        qt_progress_dialog_set_range(qt_progress_dialog_t pd,
                                          int minimum, int maximum);
void        qt_progress_dialog_set_label_text(qt_progress_dialog_t pd,
                                               const char* text);
int         qt_progress_dialog_was_canceled(qt_progress_dialog_t pd);
void        qt_progress_dialog_set_minimum_duration(qt_progress_dialog_t pd,
                                                     int msecs);
void        qt_progress_dialog_set_auto_close(qt_progress_dialog_t pd, int enabled);
void        qt_progress_dialog_set_auto_reset(qt_progress_dialog_t pd, int enabled);
void        qt_progress_dialog_reset(qt_progress_dialog_t pd);
void        qt_progress_dialog_on_canceled(qt_progress_dialog_t pd,
                                            qt_callback_void callback,
                                            long callback_id);

/* --- Input Dialog (static convenience) --- */
const char* qt_input_dialog_get_text(qt_widget_t parent, const char* title,
                                      const char* label, const char* default_text);
int         qt_input_dialog_get_int(qt_widget_t parent, const char* title,
                                     const char* label, int value,
                                     int min_val, int max_val, int step);
double      qt_input_dialog_get_double(qt_widget_t parent, const char* title,
                                        const char* label, double value,
                                        double min_val, double max_val,
                                        int decimals);
const char* qt_input_dialog_get_item(qt_widget_t parent, const char* title,
                                      const char* label, const char* items_newline,
                                      int current, int editable);
int         qt_input_dialog_was_accepted(void);

/* ========== Phase 4: Advanced Widgets ========== */

typedef void* qt_list_widget_t;
typedef void* qt_table_widget_t;
typedef void* qt_tab_widget_t;
typedef void* qt_progress_bar_t;
typedef void* qt_slider_t;

/* --- List Widget --- */
qt_list_widget_t qt_list_widget_create(qt_widget_t parent);
void        qt_list_widget_add_item(qt_list_widget_t l, const char* text);
void        qt_list_widget_insert_item(qt_list_widget_t l, int row, const char* text);
void        qt_list_widget_remove_item(qt_list_widget_t l, int row);
int         qt_list_widget_current_row(qt_list_widget_t l);
void        qt_list_widget_set_current_row(qt_list_widget_t l, int row);
const char* qt_list_widget_item_text(qt_list_widget_t l, int row);
int         qt_list_widget_count(qt_list_widget_t l);
void        qt_list_widget_clear(qt_list_widget_t l);
void        qt_list_widget_on_current_row_changed(qt_list_widget_t l,
                                                   qt_callback_int callback,
                                                   long callback_id);
void        qt_list_widget_on_item_double_clicked(qt_list_widget_t l,
                                                   qt_callback_int callback,
                                                   long callback_id);

/* --- Table Widget --- */
qt_table_widget_t qt_table_widget_create(int rows, int cols, qt_widget_t parent);
void        qt_table_widget_set_item(qt_table_widget_t t, int row, int col,
                                      const char* text);
const char* qt_table_widget_item_text(qt_table_widget_t t, int row, int col);
void        qt_table_widget_set_horizontal_header_item(qt_table_widget_t t,
                                                        int col, const char* text);
void        qt_table_widget_set_vertical_header_item(qt_table_widget_t t,
                                                      int row, const char* text);
void        qt_table_widget_set_row_count(qt_table_widget_t t, int count);
void        qt_table_widget_set_column_count(qt_table_widget_t t, int count);
int         qt_table_widget_row_count(qt_table_widget_t t);
int         qt_table_widget_column_count(qt_table_widget_t t);
int         qt_table_widget_current_row(qt_table_widget_t t);
int         qt_table_widget_current_column(qt_table_widget_t t);
void        qt_table_widget_clear(qt_table_widget_t t);
void        qt_table_widget_on_cell_clicked(qt_table_widget_t t,
                                             qt_callback_void callback,
                                             long callback_id);

/* --- Tab Widget --- */
qt_tab_widget_t qt_tab_widget_create(qt_widget_t parent);
int         qt_tab_widget_add_tab(qt_tab_widget_t t, qt_widget_t page,
                                   const char* label);
void        qt_tab_widget_set_current_index(qt_tab_widget_t t, int index);
int         qt_tab_widget_current_index(qt_tab_widget_t t);
int         qt_tab_widget_count(qt_tab_widget_t t);
void        qt_tab_widget_set_tab_text(qt_tab_widget_t t, int index,
                                        const char* text);
void        qt_tab_widget_on_current_changed(qt_tab_widget_t t,
                                              qt_callback_int callback,
                                              long callback_id);

/* --- Progress Bar --- */
qt_progress_bar_t qt_progress_bar_create(qt_widget_t parent);
void        qt_progress_bar_set_value(qt_progress_bar_t p, int value);
int         qt_progress_bar_value(qt_progress_bar_t p);
void        qt_progress_bar_set_range(qt_progress_bar_t p, int minimum, int maximum);
void        qt_progress_bar_set_format(qt_progress_bar_t p, const char* format);

/* --- Slider --- */
qt_slider_t qt_slider_create(int orientation, qt_widget_t parent);
void        qt_slider_set_value(qt_slider_t s, int value);
int         qt_slider_value(qt_slider_t s);
void        qt_slider_set_range(qt_slider_t s, int minimum, int maximum);
void        qt_slider_set_single_step(qt_slider_t s, int step);
void        qt_slider_set_tick_interval(qt_slider_t s, int interval);
void        qt_slider_set_tick_position(qt_slider_t s, int position);
void        qt_slider_on_value_changed(qt_slider_t s,
                                        qt_callback_int callback,
                                        long callback_id);

#ifdef __cplusplus
}
#endif
#endif /* QT_SHIM_H */
