#include "qt_shim.h"

#include <QApplication>
#include <QMainWindow>
#include <QWidget>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QLabel>
#include <QPushButton>
#include <QFont>
#include <QLineEdit>
#include <QCheckBox>
#include <QComboBox>
#include <QTextEdit>
#include <QSpinBox>
#include <QDialog>
#include <QMessageBox>
#include <QFileDialog>
#include <QMenuBar>
#include <QMenu>
#include <QAction>
#include <QToolBar>
#include <QStatusBar>
#include <QKeySequence>
#include <QListWidget>
#include <QListWidgetItem>
#include <QTableWidget>
#include <QTableWidgetItem>
#include <QTabWidget>
#include <QProgressBar>
#include <QSlider>
#include <QGridLayout>
#include <QTimer>
#include <QClipboard>
#include <QTreeWidget>
#include <QTreeWidgetItem>
#include <QHeaderView>
#include <QScrollArea>
#include <QSplitter>
#include <QEvent>
#include <QKeyEvent>
#include <QPixmap>
#include <QIcon>
#include <QRadioButton>
#include <QButtonGroup>
#include <QGroupBox>
#include <QFontDialog>
#include <QColorDialog>
#include <QColor>
#include <QStackedWidget>
#include <QDockWidget>
#include <QSystemTrayIcon>
#include <QPainter>
#include <QDrag>
#include <QMimeData>
#include <QDropEvent>
#include <QDragEnterEvent>
#include <QDoubleSpinBox>
#include <QDateEdit>
#include <QTimeEdit>
#include <QFrame>
#include <QProgressDialog>
#include <QInputDialog>
#include <QFormLayout>
#include <QShortcut>
#include <QTextBrowser>
#include <QDialogButtonBox>
#include <QCalendarWidget>
#include <string>
#include <cstdio>

// Storage for argc/argv (Qt requires stable pointers for QApplication lifetime)
static int    s_argc = 1;
static char   s_arg0[] = "gerbil-qt";
static char*  s_argv[] = { s_arg0, nullptr };

// Thread-local string buffer for returning strings to FFI safely.
// Qt's QString::toUtf8().constData() returns a pointer to a temporary;
// we copy into this buffer so the pointer remains valid until the next call.
static thread_local std::string s_return_buf;

// Thread-local storage for last key event (separate from s_return_buf)
static thread_local int s_last_key_code = 0;
static thread_local int s_last_key_modifiers = 0;
static thread_local std::string s_last_key_text;

// Thread-local storage for QInputDialog ok/cancel flag
static thread_local bool s_last_input_ok = false;

// KeyPressFilter: QObject subclass that intercepts key events.
// No Q_OBJECT macro needed — just overrides the virtual eventFilter method.
// Parented to the widget it filters (auto-destroyed by Qt ownership).
class KeyPressFilter : public QObject {
public:
    KeyPressFilter(QObject* parent, qt_callback_void callback, long callback_id)
        : QObject(parent), m_callback(callback), m_callback_id(callback_id) {}

    bool eventFilter(QObject* obj, QEvent* event) override {
        if (event->type() == QEvent::KeyPress) {
            auto* ke = static_cast<QKeyEvent*>(event);
            s_last_key_code = ke->key();
            s_last_key_modifiers = static_cast<int>(ke->modifiers());
            s_last_key_text = ke->text().toUtf8().toStdString();
            m_callback(m_callback_id);
        }
        return QObject::eventFilter(obj, event);
    }

private:
    qt_callback_void m_callback;
    long m_callback_id;
};


// ============================================================
// Application lifecycle
// ============================================================

extern "C" qt_application_t qt_application_create(int argc, char** argv) {
    // QApplication takes argc by REFERENCE — must use static storage.
    // Always use our static argc/argv to avoid dangling references.
    (void)argc; (void)argv;
    return new QApplication(s_argc, s_argv);
}

extern "C" int qt_application_exec(qt_application_t app) {
    return static_cast<QApplication*>(app)->exec();
}

extern "C" void qt_application_quit(qt_application_t app) {
    static_cast<QApplication*>(app)->quit();
}

extern "C" void qt_application_process_events(qt_application_t app) {
    static_cast<QApplication*>(app)->processEvents();
}

extern "C" void qt_application_destroy(qt_application_t app) {
    delete static_cast<QApplication*>(app);
}

// ============================================================
// Widget base
// ============================================================

extern "C" qt_widget_t qt_widget_create(qt_widget_t parent) {
    return new QWidget(static_cast<QWidget*>(parent));
}

extern "C" void qt_widget_show(qt_widget_t w) {
    static_cast<QWidget*>(w)->show();
}

extern "C" void qt_widget_hide(qt_widget_t w) {
    static_cast<QWidget*>(w)->hide();
}

extern "C" void qt_widget_close(qt_widget_t w) {
    static_cast<QWidget*>(w)->close();
}

extern "C" void qt_widget_set_enabled(qt_widget_t w, int enabled) {
    static_cast<QWidget*>(w)->setEnabled(enabled != 0);
}

extern "C" int qt_widget_is_enabled(qt_widget_t w) {
    return static_cast<QWidget*>(w)->isEnabled() ? 1 : 0;
}

extern "C" void qt_widget_set_visible(qt_widget_t w, int visible) {
    static_cast<QWidget*>(w)->setVisible(visible != 0);
}

extern "C" void qt_widget_set_fixed_size(qt_widget_t w, int width, int height) {
    static_cast<QWidget*>(w)->setFixedSize(width, height);
}

extern "C" void qt_widget_set_minimum_size(qt_widget_t w, int width, int height) {
    static_cast<QWidget*>(w)->setMinimumSize(width, height);
}

extern "C" void qt_widget_set_maximum_size(qt_widget_t w, int width, int height) {
    static_cast<QWidget*>(w)->setMaximumSize(width, height);
}

extern "C" void qt_widget_resize(qt_widget_t w, int width, int height) {
    static_cast<QWidget*>(w)->resize(width, height);
}

extern "C" void qt_widget_set_style_sheet(qt_widget_t w, const char* css) {
    static_cast<QWidget*>(w)->setStyleSheet(QString::fromUtf8(css));
}

extern "C" void qt_widget_set_tooltip(qt_widget_t w, const char* text) {
    static_cast<QWidget*>(w)->setToolTip(QString::fromUtf8(text));
}

extern "C" void qt_widget_set_font_size(qt_widget_t w, int size) {
    QFont font = static_cast<QWidget*>(w)->font();
    font.setPointSize(size);
    static_cast<QWidget*>(w)->setFont(font);
}

extern "C" void qt_widget_destroy(qt_widget_t w) {
    delete static_cast<QWidget*>(w);
}

// ============================================================
// Main Window
// ============================================================

extern "C" qt_main_window_t qt_main_window_create(qt_widget_t parent) {
    return new QMainWindow(static_cast<QWidget*>(parent));
}

extern "C" void qt_main_window_set_title(qt_main_window_t w, const char* title) {
    static_cast<QMainWindow*>(w)->setWindowTitle(QString::fromUtf8(title));
}

extern "C" void qt_main_window_set_central_widget(qt_main_window_t w, qt_widget_t child) {
    static_cast<QMainWindow*>(w)->setCentralWidget(static_cast<QWidget*>(child));
}

// ============================================================
// Layouts
// ============================================================

extern "C" qt_layout_t qt_vbox_layout_create(qt_widget_t parent) {
    return new QVBoxLayout(static_cast<QWidget*>(parent));
}

extern "C" qt_layout_t qt_hbox_layout_create(qt_widget_t parent) {
    return new QHBoxLayout(static_cast<QWidget*>(parent));
}

extern "C" void qt_layout_add_widget(qt_layout_t layout, qt_widget_t widget) {
    static_cast<QLayout*>(layout)->addWidget(static_cast<QWidget*>(widget));
}

extern "C" void qt_layout_add_stretch(qt_layout_t layout, int stretch) {
    // addStretch is on QBoxLayout, not QLayout
    if (auto* box = dynamic_cast<QBoxLayout*>(static_cast<QLayout*>(layout))) {
        box->addStretch(stretch);
    }
}

extern "C" void qt_layout_set_spacing(qt_layout_t layout, int spacing) {
    static_cast<QLayout*>(layout)->setSpacing(spacing);
}

extern "C" void qt_layout_set_margins(qt_layout_t layout, int left, int top,
                                       int right, int bottom) {
    static_cast<QLayout*>(layout)->setContentsMargins(left, top, right, bottom);
}

// ============================================================
// Labels
// ============================================================

extern "C" qt_label_t qt_label_create(const char* text, qt_widget_t parent) {
    return new QLabel(QString::fromUtf8(text), static_cast<QWidget*>(parent));
}

extern "C" void qt_label_set_text(qt_label_t l, const char* text) {
    static_cast<QLabel*>(l)->setText(QString::fromUtf8(text));
}

extern "C" const char* qt_label_text(qt_label_t l) {
    s_return_buf = static_cast<QLabel*>(l)->text().toUtf8().toStdString();
    return s_return_buf.c_str();
}

extern "C" void qt_label_set_alignment(qt_label_t l, int alignment) {
    static_cast<QLabel*>(l)->setAlignment(static_cast<Qt::Alignment>(alignment));
}

// ============================================================
// Push Button
// ============================================================

extern "C" qt_push_button_t qt_push_button_create(const char* text, qt_widget_t parent) {
    return new QPushButton(QString::fromUtf8(text), static_cast<QWidget*>(parent));
}

extern "C" void qt_push_button_set_text(qt_push_button_t b, const char* text) {
    static_cast<QPushButton*>(b)->setText(QString::fromUtf8(text));
}

extern "C" const char* qt_push_button_text(qt_push_button_t b) {
    s_return_buf = static_cast<QPushButton*>(b)->text().toUtf8().toStdString();
    return s_return_buf.c_str();
}

extern "C" void qt_push_button_on_clicked(qt_push_button_t b,
                                          qt_callback_void callback,
                                          long callback_id) {
    QObject::connect(static_cast<QPushButton*>(b), &QPushButton::clicked,
                     [callback, callback_id]() {
                         callback(callback_id);
                     });
}

// ============================================================
// Line Edit
// ============================================================

extern "C" qt_line_edit_t qt_line_edit_create(qt_widget_t parent) {
    return new QLineEdit(static_cast<QWidget*>(parent));
}

extern "C" void qt_line_edit_set_text(qt_line_edit_t e, const char* text) {
    static_cast<QLineEdit*>(e)->setText(QString::fromUtf8(text));
}

extern "C" const char* qt_line_edit_text(qt_line_edit_t e) {
    s_return_buf = static_cast<QLineEdit*>(e)->text().toUtf8().toStdString();
    return s_return_buf.c_str();
}

extern "C" void qt_line_edit_set_placeholder(qt_line_edit_t e, const char* text) {
    static_cast<QLineEdit*>(e)->setPlaceholderText(QString::fromUtf8(text));
}

extern "C" void qt_line_edit_set_read_only(qt_line_edit_t e, int read_only) {
    static_cast<QLineEdit*>(e)->setReadOnly(read_only != 0);
}

extern "C" void qt_line_edit_set_echo_mode(qt_line_edit_t e, int mode) {
    static_cast<QLineEdit*>(e)->setEchoMode(static_cast<QLineEdit::EchoMode>(mode));
}

extern "C" void qt_line_edit_on_text_changed(qt_line_edit_t e,
                                              qt_callback_string callback,
                                              long callback_id) {
    QObject::connect(static_cast<QLineEdit*>(e), &QLineEdit::textChanged,
                     [callback, callback_id](const QString& text) {
                         s_return_buf = text.toUtf8().toStdString();
                         callback(callback_id, s_return_buf.c_str());
                     });
}

extern "C" void qt_line_edit_on_return_pressed(qt_line_edit_t e,
                                                qt_callback_void callback,
                                                long callback_id) {
    QObject::connect(static_cast<QLineEdit*>(e), &QLineEdit::returnPressed,
                     [callback, callback_id]() {
                         callback(callback_id);
                     });
}

// ============================================================
// Check Box
// ============================================================

extern "C" qt_check_box_t qt_check_box_create(const char* text, qt_widget_t parent) {
    return new QCheckBox(QString::fromUtf8(text), static_cast<QWidget*>(parent));
}

extern "C" void qt_check_box_set_text(qt_check_box_t c, const char* text) {
    static_cast<QCheckBox*>(c)->setText(QString::fromUtf8(text));
}

extern "C" void qt_check_box_set_checked(qt_check_box_t c, int checked) {
    static_cast<QCheckBox*>(c)->setChecked(checked != 0);
}

extern "C" int qt_check_box_is_checked(qt_check_box_t c) {
    return static_cast<QCheckBox*>(c)->isChecked() ? 1 : 0;
}

extern "C" void qt_check_box_on_toggled(qt_check_box_t c,
                                         qt_callback_bool callback,
                                         long callback_id) {
    QObject::connect(static_cast<QCheckBox*>(c), &QCheckBox::toggled,
                     [callback, callback_id](bool checked) {
                         callback(callback_id, checked ? 1 : 0);
                     });
}

// ============================================================
// Combo Box
// ============================================================

extern "C" qt_combo_box_t qt_combo_box_create(qt_widget_t parent) {
    return new QComboBox(static_cast<QWidget*>(parent));
}

extern "C" void qt_combo_box_add_item(qt_combo_box_t c, const char* text) {
    static_cast<QComboBox*>(c)->addItem(QString::fromUtf8(text));
}

extern "C" void qt_combo_box_set_current_index(qt_combo_box_t c, int index) {
    static_cast<QComboBox*>(c)->setCurrentIndex(index);
}

extern "C" int qt_combo_box_current_index(qt_combo_box_t c) {
    return static_cast<QComboBox*>(c)->currentIndex();
}

extern "C" const char* qt_combo_box_current_text(qt_combo_box_t c) {
    s_return_buf = static_cast<QComboBox*>(c)->currentText().toUtf8().toStdString();
    return s_return_buf.c_str();
}

extern "C" int qt_combo_box_count(qt_combo_box_t c) {
    return static_cast<QComboBox*>(c)->count();
}

extern "C" void qt_combo_box_clear(qt_combo_box_t c) {
    static_cast<QComboBox*>(c)->clear();
}

extern "C" void qt_combo_box_on_current_index_changed(qt_combo_box_t c,
                                                       qt_callback_int callback,
                                                       long callback_id) {
    QObject::connect(static_cast<QComboBox*>(c),
                     QOverload<int>::of(&QComboBox::currentIndexChanged),
                     [callback, callback_id](int index) {
                         callback(callback_id, index);
                     });
}

// ============================================================
// Text Edit
// ============================================================

extern "C" qt_text_edit_t qt_text_edit_create(qt_widget_t parent) {
    return new QTextEdit(static_cast<QWidget*>(parent));
}

extern "C" void qt_text_edit_set_text(qt_text_edit_t e, const char* text) {
    static_cast<QTextEdit*>(e)->setPlainText(QString::fromUtf8(text));
}

extern "C" const char* qt_text_edit_text(qt_text_edit_t e) {
    s_return_buf = static_cast<QTextEdit*>(e)->toPlainText().toUtf8().toStdString();
    return s_return_buf.c_str();
}

extern "C" void qt_text_edit_set_placeholder(qt_text_edit_t e, const char* text) {
    static_cast<QTextEdit*>(e)->setPlaceholderText(QString::fromUtf8(text));
}

extern "C" void qt_text_edit_set_read_only(qt_text_edit_t e, int read_only) {
    static_cast<QTextEdit*>(e)->setReadOnly(read_only != 0);
}

extern "C" void qt_text_edit_append(qt_text_edit_t e, const char* text) {
    static_cast<QTextEdit*>(e)->append(QString::fromUtf8(text));
}

extern "C" void qt_text_edit_clear(qt_text_edit_t e) {
    static_cast<QTextEdit*>(e)->clear();
}

extern "C" void qt_text_edit_on_text_changed(qt_text_edit_t e,
                                              qt_callback_void callback,
                                              long callback_id) {
    QObject::connect(static_cast<QTextEdit*>(e), &QTextEdit::textChanged,
                     [callback, callback_id]() {
                         callback(callback_id);
                     });
}

// ============================================================
// Spin Box
// ============================================================

extern "C" qt_spin_box_t qt_spin_box_create(qt_widget_t parent) {
    return new QSpinBox(static_cast<QWidget*>(parent));
}

extern "C" void qt_spin_box_set_value(qt_spin_box_t s, int value) {
    static_cast<QSpinBox*>(s)->setValue(value);
}

extern "C" int qt_spin_box_value(qt_spin_box_t s) {
    return static_cast<QSpinBox*>(s)->value();
}

extern "C" void qt_spin_box_set_range(qt_spin_box_t s, int minimum, int maximum) {
    static_cast<QSpinBox*>(s)->setRange(minimum, maximum);
}

extern "C" void qt_spin_box_set_single_step(qt_spin_box_t s, int step) {
    static_cast<QSpinBox*>(s)->setSingleStep(step);
}

extern "C" void qt_spin_box_set_prefix(qt_spin_box_t s, const char* prefix) {
    static_cast<QSpinBox*>(s)->setPrefix(QString::fromUtf8(prefix));
}

extern "C" void qt_spin_box_set_suffix(qt_spin_box_t s, const char* suffix) {
    static_cast<QSpinBox*>(s)->setSuffix(QString::fromUtf8(suffix));
}

extern "C" void qt_spin_box_on_value_changed(qt_spin_box_t s,
                                              qt_callback_int callback,
                                              long callback_id) {
    QObject::connect(static_cast<QSpinBox*>(s),
                     QOverload<int>::of(&QSpinBox::valueChanged),
                     [callback, callback_id](int value) {
                         callback(callback_id, value);
                     });
}

// ============================================================
// Dialog
// ============================================================

extern "C" qt_dialog_t qt_dialog_create(qt_widget_t parent) {
    return new QDialog(static_cast<QWidget*>(parent));
}

extern "C" int qt_dialog_exec(qt_dialog_t d) {
    return static_cast<QDialog*>(d)->exec();
}

extern "C" void qt_dialog_accept(qt_dialog_t d) {
    static_cast<QDialog*>(d)->accept();
}

extern "C" void qt_dialog_reject(qt_dialog_t d) {
    static_cast<QDialog*>(d)->reject();
}

extern "C" void qt_dialog_set_title(qt_dialog_t d, const char* title) {
    static_cast<QDialog*>(d)->setWindowTitle(QString::fromUtf8(title));
}

// ============================================================
// Message Box (static convenience)
// ============================================================

extern "C" int qt_message_box_information(qt_widget_t parent,
                                           const char* title, const char* text) {
    return QMessageBox::information(static_cast<QWidget*>(parent),
                                    QString::fromUtf8(title),
                                    QString::fromUtf8(text));
}

extern "C" int qt_message_box_warning(qt_widget_t parent,
                                       const char* title, const char* text) {
    return QMessageBox::warning(static_cast<QWidget*>(parent),
                                QString::fromUtf8(title),
                                QString::fromUtf8(text));
}

extern "C" int qt_message_box_question(qt_widget_t parent,
                                        const char* title, const char* text) {
    return QMessageBox::question(static_cast<QWidget*>(parent),
                                 QString::fromUtf8(title),
                                 QString::fromUtf8(text));
}

extern "C" int qt_message_box_critical(qt_widget_t parent,
                                        const char* title, const char* text) {
    return QMessageBox::critical(static_cast<QWidget*>(parent),
                                 QString::fromUtf8(title),
                                 QString::fromUtf8(text));
}

// ============================================================
// File Dialog (static convenience)
// ============================================================

extern "C" const char* qt_file_dialog_open_file(qt_widget_t parent,
                                                 const char* caption,
                                                 const char* dir,
                                                 const char* filter) {
    s_return_buf = QFileDialog::getOpenFileName(
        static_cast<QWidget*>(parent),
        QString::fromUtf8(caption),
        QString::fromUtf8(dir),
        QString::fromUtf8(filter)).toUtf8().toStdString();
    return s_return_buf.c_str();
}

extern "C" const char* qt_file_dialog_save_file(qt_widget_t parent,
                                                 const char* caption,
                                                 const char* dir,
                                                 const char* filter) {
    s_return_buf = QFileDialog::getSaveFileName(
        static_cast<QWidget*>(parent),
        QString::fromUtf8(caption),
        QString::fromUtf8(dir),
        QString::fromUtf8(filter)).toUtf8().toStdString();
    return s_return_buf.c_str();
}

extern "C" const char* qt_file_dialog_open_directory(qt_widget_t parent,
                                                      const char* caption,
                                                      const char* dir) {
    s_return_buf = QFileDialog::getExistingDirectory(
        static_cast<QWidget*>(parent),
        QString::fromUtf8(caption),
        QString::fromUtf8(dir)).toUtf8().toStdString();
    return s_return_buf.c_str();
}

// ============================================================
// Menu Bar
// ============================================================

extern "C" qt_menu_bar_t qt_main_window_menu_bar(qt_main_window_t w) {
    return static_cast<QMainWindow*>(w)->menuBar();
}

// ============================================================
// Menu
// ============================================================

extern "C" qt_menu_t qt_menu_bar_add_menu(qt_menu_bar_t bar, const char* title) {
    return static_cast<QMenuBar*>(bar)->addMenu(QString::fromUtf8(title));
}

extern "C" qt_menu_t qt_menu_add_menu(qt_menu_t menu, const char* title) {
    return static_cast<QMenu*>(menu)->addMenu(QString::fromUtf8(title));
}

extern "C" void qt_menu_add_action(qt_menu_t menu, qt_action_t action) {
    static_cast<QMenu*>(menu)->addAction(static_cast<QAction*>(action));
}

extern "C" void qt_menu_add_separator(qt_menu_t menu) {
    static_cast<QMenu*>(menu)->addSeparator();
}

// ============================================================
// Action
// ============================================================

extern "C" qt_action_t qt_action_create(const char* text, qt_widget_t parent) {
    return new QAction(QString::fromUtf8(text), static_cast<QWidget*>(parent));
}

extern "C" void qt_action_set_text(qt_action_t a, const char* text) {
    static_cast<QAction*>(a)->setText(QString::fromUtf8(text));
}

extern "C" const char* qt_action_text(qt_action_t a) {
    s_return_buf = static_cast<QAction*>(a)->text().toUtf8().toStdString();
    return s_return_buf.c_str();
}

extern "C" void qt_action_set_shortcut(qt_action_t a, const char* shortcut) {
    static_cast<QAction*>(a)->setShortcut(QKeySequence(QString::fromUtf8(shortcut)));
}

extern "C" void qt_action_set_enabled(qt_action_t a, int enabled) {
    static_cast<QAction*>(a)->setEnabled(enabled != 0);
}

extern "C" int qt_action_is_enabled(qt_action_t a) {
    return static_cast<QAction*>(a)->isEnabled() ? 1 : 0;
}

extern "C" void qt_action_set_checkable(qt_action_t a, int checkable) {
    static_cast<QAction*>(a)->setCheckable(checkable != 0);
}

extern "C" int qt_action_is_checkable(qt_action_t a) {
    return static_cast<QAction*>(a)->isCheckable() ? 1 : 0;
}

extern "C" void qt_action_set_checked(qt_action_t a, int checked) {
    static_cast<QAction*>(a)->setChecked(checked != 0);
}

extern "C" int qt_action_is_checked(qt_action_t a) {
    return static_cast<QAction*>(a)->isChecked() ? 1 : 0;
}

extern "C" void qt_action_set_tooltip(qt_action_t a, const char* text) {
    static_cast<QAction*>(a)->setToolTip(QString::fromUtf8(text));
}

extern "C" void qt_action_set_status_tip(qt_action_t a, const char* text) {
    static_cast<QAction*>(a)->setStatusTip(QString::fromUtf8(text));
}

extern "C" void qt_action_on_triggered(qt_action_t a,
                                        qt_callback_void callback,
                                        long callback_id) {
    QObject::connect(static_cast<QAction*>(a), &QAction::triggered,
                     [callback, callback_id]() {
                         callback(callback_id);
                     });
}

extern "C" void qt_action_on_toggled(qt_action_t a,
                                      qt_callback_bool callback,
                                      long callback_id) {
    QObject::connect(static_cast<QAction*>(a), &QAction::toggled,
                     [callback, callback_id](bool checked) {
                         callback(callback_id, checked ? 1 : 0);
                     });
}

// ============================================================
// Toolbar
// ============================================================

extern "C" qt_toolbar_t qt_toolbar_create(const char* title, qt_widget_t parent) {
    return new QToolBar(QString::fromUtf8(title), static_cast<QWidget*>(parent));
}

extern "C" void qt_main_window_add_toolbar(qt_main_window_t w, qt_toolbar_t tb) {
    static_cast<QMainWindow*>(w)->addToolBar(static_cast<QToolBar*>(tb));
}

extern "C" void qt_toolbar_add_action(qt_toolbar_t tb, qt_action_t action) {
    static_cast<QToolBar*>(tb)->addAction(static_cast<QAction*>(action));
}

extern "C" void qt_toolbar_add_separator(qt_toolbar_t tb) {
    static_cast<QToolBar*>(tb)->addSeparator();
}

extern "C" void qt_toolbar_add_widget(qt_toolbar_t tb, qt_widget_t w) {
    static_cast<QToolBar*>(tb)->addWidget(static_cast<QWidget*>(w));
}

extern "C" void qt_toolbar_set_movable(qt_toolbar_t tb, int movable) {
    static_cast<QToolBar*>(tb)->setMovable(movable != 0);
}

extern "C" void qt_toolbar_set_icon_size(qt_toolbar_t tb, int width, int height) {
    static_cast<QToolBar*>(tb)->setIconSize(QSize(width, height));
}

// ============================================================
// Status Bar
// ============================================================

extern "C" void qt_main_window_set_status_bar_text(qt_main_window_t w,
                                                     const char* text) {
    static_cast<QMainWindow*>(w)->statusBar()->showMessage(QString::fromUtf8(text));
}

// ============================================================
// List Widget
// ============================================================

extern "C" qt_list_widget_t qt_list_widget_create(qt_widget_t parent) {
    return new QListWidget(static_cast<QWidget*>(parent));
}

extern "C" void qt_list_widget_add_item(qt_list_widget_t l, const char* text) {
    static_cast<QListWidget*>(l)->addItem(QString::fromUtf8(text));
}

extern "C" void qt_list_widget_insert_item(qt_list_widget_t l, int row,
                                            const char* text) {
    static_cast<QListWidget*>(l)->insertItem(row, QString::fromUtf8(text));
}

extern "C" void qt_list_widget_remove_item(qt_list_widget_t l, int row) {
    auto* list = static_cast<QListWidget*>(l);
    delete list->takeItem(row);
}

extern "C" int qt_list_widget_current_row(qt_list_widget_t l) {
    return static_cast<QListWidget*>(l)->currentRow();
}

extern "C" void qt_list_widget_set_current_row(qt_list_widget_t l, int row) {
    static_cast<QListWidget*>(l)->setCurrentRow(row);
}

extern "C" const char* qt_list_widget_item_text(qt_list_widget_t l, int row) {
    auto* item = static_cast<QListWidget*>(l)->item(row);
    if (item) {
        s_return_buf = item->text().toUtf8().toStdString();
    } else {
        s_return_buf.clear();
    }
    return s_return_buf.c_str();
}

extern "C" int qt_list_widget_count(qt_list_widget_t l) {
    return static_cast<QListWidget*>(l)->count();
}

extern "C" void qt_list_widget_clear(qt_list_widget_t l) {
    static_cast<QListWidget*>(l)->clear();
}

extern "C" void qt_list_widget_on_current_row_changed(qt_list_widget_t l,
                                                       qt_callback_int callback,
                                                       long callback_id) {
    QObject::connect(static_cast<QListWidget*>(l),
                     &QListWidget::currentRowChanged,
                     [callback, callback_id](int row) {
                         callback(callback_id, row);
                     });
}

extern "C" void qt_list_widget_on_item_double_clicked(qt_list_widget_t l,
                                                       qt_callback_int callback,
                                                       long callback_id) {
    auto* list = static_cast<QListWidget*>(l);
    QObject::connect(list, &QListWidget::itemDoubleClicked,
                     [callback, callback_id, list](QListWidgetItem* item) {
                         callback(callback_id, list->row(item));
                     });
}

// ============================================================
// Table Widget
// ============================================================

extern "C" qt_table_widget_t qt_table_widget_create(int rows, int cols,
                                                      qt_widget_t parent) {
    return new QTableWidget(rows, cols, static_cast<QWidget*>(parent));
}

extern "C" void qt_table_widget_set_item(qt_table_widget_t t, int row, int col,
                                          const char* text) {
    static_cast<QTableWidget*>(t)->setItem(
        row, col, new QTableWidgetItem(QString::fromUtf8(text)));
}

extern "C" const char* qt_table_widget_item_text(qt_table_widget_t t,
                                                   int row, int col) {
    auto* item = static_cast<QTableWidget*>(t)->item(row, col);
    if (item) {
        s_return_buf = item->text().toUtf8().toStdString();
    } else {
        s_return_buf.clear();
    }
    return s_return_buf.c_str();
}

extern "C" void qt_table_widget_set_horizontal_header_item(qt_table_widget_t t,
                                                             int col,
                                                             const char* text) {
    static_cast<QTableWidget*>(t)->setHorizontalHeaderItem(
        col, new QTableWidgetItem(QString::fromUtf8(text)));
}

extern "C" void qt_table_widget_set_vertical_header_item(qt_table_widget_t t,
                                                           int row,
                                                           const char* text) {
    static_cast<QTableWidget*>(t)->setVerticalHeaderItem(
        row, new QTableWidgetItem(QString::fromUtf8(text)));
}

extern "C" void qt_table_widget_set_row_count(qt_table_widget_t t, int count) {
    static_cast<QTableWidget*>(t)->setRowCount(count);
}

extern "C" void qt_table_widget_set_column_count(qt_table_widget_t t, int count) {
    static_cast<QTableWidget*>(t)->setColumnCount(count);
}

extern "C" int qt_table_widget_row_count(qt_table_widget_t t) {
    return static_cast<QTableWidget*>(t)->rowCount();
}

extern "C" int qt_table_widget_column_count(qt_table_widget_t t) {
    return static_cast<QTableWidget*>(t)->columnCount();
}

extern "C" int qt_table_widget_current_row(qt_table_widget_t t) {
    return static_cast<QTableWidget*>(t)->currentRow();
}

extern "C" int qt_table_widget_current_column(qt_table_widget_t t) {
    return static_cast<QTableWidget*>(t)->currentColumn();
}

extern "C" void qt_table_widget_clear(qt_table_widget_t t) {
    static_cast<QTableWidget*>(t)->clear();
}

extern "C" void qt_table_widget_on_cell_clicked(qt_table_widget_t t,
                                                  qt_callback_void callback,
                                                  long callback_id) {
    QObject::connect(static_cast<QTableWidget*>(t),
                     &QTableWidget::cellClicked,
                     [callback, callback_id](int, int) {
                         callback(callback_id);
                     });
}

// ============================================================
// Tab Widget
// ============================================================

extern "C" qt_tab_widget_t qt_tab_widget_create(qt_widget_t parent) {
    return new QTabWidget(static_cast<QWidget*>(parent));
}

extern "C" int qt_tab_widget_add_tab(qt_tab_widget_t t, qt_widget_t page,
                                      const char* label) {
    return static_cast<QTabWidget*>(t)->addTab(
        static_cast<QWidget*>(page), QString::fromUtf8(label));
}

extern "C" void qt_tab_widget_set_current_index(qt_tab_widget_t t, int index) {
    static_cast<QTabWidget*>(t)->setCurrentIndex(index);
}

extern "C" int qt_tab_widget_current_index(qt_tab_widget_t t) {
    return static_cast<QTabWidget*>(t)->currentIndex();
}

extern "C" int qt_tab_widget_count(qt_tab_widget_t t) {
    return static_cast<QTabWidget*>(t)->count();
}

extern "C" void qt_tab_widget_set_tab_text(qt_tab_widget_t t, int index,
                                             const char* text) {
    static_cast<QTabWidget*>(t)->setTabText(index, QString::fromUtf8(text));
}

extern "C" void qt_tab_widget_on_current_changed(qt_tab_widget_t t,
                                                   qt_callback_int callback,
                                                   long callback_id) {
    QObject::connect(static_cast<QTabWidget*>(t),
                     &QTabWidget::currentChanged,
                     [callback, callback_id](int index) {
                         callback(callback_id, index);
                     });
}

// ============================================================
// Progress Bar
// ============================================================

extern "C" qt_progress_bar_t qt_progress_bar_create(qt_widget_t parent) {
    return new QProgressBar(static_cast<QWidget*>(parent));
}

extern "C" void qt_progress_bar_set_value(qt_progress_bar_t p, int value) {
    static_cast<QProgressBar*>(p)->setValue(value);
}

extern "C" int qt_progress_bar_value(qt_progress_bar_t p) {
    return static_cast<QProgressBar*>(p)->value();
}

extern "C" void qt_progress_bar_set_range(qt_progress_bar_t p,
                                            int minimum, int maximum) {
    static_cast<QProgressBar*>(p)->setRange(minimum, maximum);
}

extern "C" void qt_progress_bar_set_format(qt_progress_bar_t p,
                                             const char* format) {
    static_cast<QProgressBar*>(p)->setFormat(QString::fromUtf8(format));
}

// ============================================================
// Slider
// ============================================================

extern "C" qt_slider_t qt_slider_create(int orientation, qt_widget_t parent) {
    return new QSlider(static_cast<Qt::Orientation>(orientation),
                       static_cast<QWidget*>(parent));
}

extern "C" void qt_slider_set_value(qt_slider_t s, int value) {
    static_cast<QSlider*>(s)->setValue(value);
}

extern "C" int qt_slider_value(qt_slider_t s) {
    return static_cast<QSlider*>(s)->value();
}

extern "C" void qt_slider_set_range(qt_slider_t s, int minimum, int maximum) {
    static_cast<QSlider*>(s)->setRange(minimum, maximum);
}

extern "C" void qt_slider_set_single_step(qt_slider_t s, int step) {
    static_cast<QSlider*>(s)->setSingleStep(step);
}

extern "C" void qt_slider_set_tick_interval(qt_slider_t s, int interval) {
    static_cast<QSlider*>(s)->setTickInterval(interval);
}

extern "C" void qt_slider_set_tick_position(qt_slider_t s, int position) {
    static_cast<QSlider*>(s)->setTickPosition(
        static_cast<QSlider::TickPosition>(position));
}

extern "C" void qt_slider_on_value_changed(qt_slider_t s,
                                             qt_callback_int callback,
                                             long callback_id) {
    QObject::connect(static_cast<QSlider*>(s), &QSlider::valueChanged,
                     [callback, callback_id](int value) {
                         callback(callback_id, value);
                     });
}

// ============================================================
// Grid Layout
// ============================================================

extern "C" qt_layout_t qt_grid_layout_create(qt_widget_t parent) {
    return new QGridLayout(static_cast<QWidget*>(parent));
}

extern "C" void qt_grid_layout_add_widget(qt_layout_t layout, qt_widget_t widget,
                                           int row, int col,
                                           int row_span, int col_span) {
    static_cast<QGridLayout*>(layout)->addWidget(
        static_cast<QWidget*>(widget), row, col, row_span, col_span);
}

extern "C" void qt_grid_layout_set_row_stretch(qt_layout_t layout,
                                                int row, int stretch) {
    static_cast<QGridLayout*>(layout)->setRowStretch(row, stretch);
}

extern "C" void qt_grid_layout_set_column_stretch(qt_layout_t layout,
                                                    int col, int stretch) {
    static_cast<QGridLayout*>(layout)->setColumnStretch(col, stretch);
}

extern "C" void qt_grid_layout_set_row_minimum_height(qt_layout_t layout,
                                                        int row, int height) {
    static_cast<QGridLayout*>(layout)->setRowMinimumHeight(row, height);
}

extern "C" void qt_grid_layout_set_column_minimum_width(qt_layout_t layout,
                                                          int col, int width) {
    static_cast<QGridLayout*>(layout)->setColumnMinimumWidth(col, width);
}

// ============================================================
// Timer
// ============================================================

extern "C" qt_timer_t qt_timer_create(void) {
    return new QTimer();
}

extern "C" void qt_timer_start(qt_timer_t t, int msec) {
    static_cast<QTimer*>(t)->start(msec);
}

extern "C" void qt_timer_stop(qt_timer_t t) {
    static_cast<QTimer*>(t)->stop();
}

extern "C" void qt_timer_set_single_shot(qt_timer_t t, int single_shot) {
    static_cast<QTimer*>(t)->setSingleShot(single_shot != 0);
}

extern "C" int qt_timer_is_active(qt_timer_t t) {
    return static_cast<QTimer*>(t)->isActive() ? 1 : 0;
}

extern "C" int qt_timer_interval(qt_timer_t t) {
    return static_cast<QTimer*>(t)->interval();
}

extern "C" void qt_timer_set_interval(qt_timer_t t, int msec) {
    static_cast<QTimer*>(t)->setInterval(msec);
}

extern "C" void qt_timer_on_timeout(qt_timer_t t,
                                     qt_callback_void callback,
                                     long callback_id) {
    QObject::connect(static_cast<QTimer*>(t), &QTimer::timeout,
                     [callback, callback_id]() {
                         callback(callback_id);
                     });
}

extern "C" void qt_timer_single_shot(int msec,
                                      qt_callback_void callback,
                                      long callback_id) {
    QTimer::singleShot(msec, [callback, callback_id]() {
        callback(callback_id);
    });
}

extern "C" void qt_timer_destroy(qt_timer_t t) {
    delete static_cast<QTimer*>(t);
}

// ============================================================
// Clipboard
// ============================================================

extern "C" const char* qt_clipboard_text(qt_application_t app) {
    (void)app;
    QClipboard* cb = QApplication::clipboard();
    s_return_buf = cb->text().toUtf8().toStdString();
    return s_return_buf.c_str();
}

extern "C" void qt_clipboard_set_text(qt_application_t app, const char* text) {
    (void)app;
    QApplication::clipboard()->setText(QString::fromUtf8(text));
}

extern "C" void qt_clipboard_on_changed(qt_application_t app,
                                         qt_callback_void callback,
                                         long callback_id) {
    (void)app;
    QObject::connect(QApplication::clipboard(), &QClipboard::dataChanged,
                     [callback, callback_id]() {
                         callback(callback_id);
                     });
}

// ============================================================
// Tree Widget
// ============================================================

extern "C" qt_tree_widget_t qt_tree_widget_create(qt_widget_t parent) {
    return new QTreeWidget(static_cast<QWidget*>(parent));
}

extern "C" void qt_tree_widget_set_column_count(qt_tree_widget_t t, int count) {
    static_cast<QTreeWidget*>(t)->setColumnCount(count);
}

extern "C" int qt_tree_widget_column_count(qt_tree_widget_t t) {
    return static_cast<QTreeWidget*>(t)->columnCount();
}

extern "C" void qt_tree_widget_set_header_label(qt_tree_widget_t t,
                                                  const char* label) {
    static_cast<QTreeWidget*>(t)->setHeaderLabel(QString::fromUtf8(label));
}

extern "C" void qt_tree_widget_set_header_item_text(qt_tree_widget_t t,
                                                      int col,
                                                      const char* text) {
    auto* tree = static_cast<QTreeWidget*>(t);
    auto* header = tree->headerItem();
    if (header) {
        header->setText(col, QString::fromUtf8(text));
    }
}

extern "C" void qt_tree_widget_add_top_level_item(qt_tree_widget_t t,
                                                    qt_tree_item_t item) {
    static_cast<QTreeWidget*>(t)->addTopLevelItem(
        static_cast<QTreeWidgetItem*>(item));
}

extern "C" int qt_tree_widget_top_level_item_count(qt_tree_widget_t t) {
    return static_cast<QTreeWidget*>(t)->topLevelItemCount();
}

extern "C" qt_tree_item_t qt_tree_widget_top_level_item(qt_tree_widget_t t,
                                                          int index) {
    return static_cast<QTreeWidget*>(t)->topLevelItem(index);
}

extern "C" qt_tree_item_t qt_tree_widget_current_item(qt_tree_widget_t t) {
    return static_cast<QTreeWidget*>(t)->currentItem();
}

extern "C" void qt_tree_widget_set_current_item(qt_tree_widget_t t,
                                                  qt_tree_item_t item) {
    static_cast<QTreeWidget*>(t)->setCurrentItem(
        static_cast<QTreeWidgetItem*>(item));
}

extern "C" void qt_tree_widget_expand_item(qt_tree_widget_t t,
                                             qt_tree_item_t item) {
    (void)t;
    static_cast<QTreeWidgetItem*>(item)->setExpanded(true);
}

extern "C" void qt_tree_widget_collapse_item(qt_tree_widget_t t,
                                               qt_tree_item_t item) {
    (void)t;
    static_cast<QTreeWidgetItem*>(item)->setExpanded(false);
}

extern "C" void qt_tree_widget_expand_all(qt_tree_widget_t t) {
    static_cast<QTreeWidget*>(t)->expandAll();
}

extern "C" void qt_tree_widget_collapse_all(qt_tree_widget_t t) {
    static_cast<QTreeWidget*>(t)->collapseAll();
}

extern "C" void qt_tree_widget_clear(qt_tree_widget_t t) {
    static_cast<QTreeWidget*>(t)->clear();
}

extern "C" void qt_tree_widget_on_current_item_changed(qt_tree_widget_t t,
                                                         qt_callback_void callback,
                                                         long callback_id) {
    QObject::connect(static_cast<QTreeWidget*>(t),
                     &QTreeWidget::currentItemChanged,
                     [callback, callback_id](QTreeWidgetItem*, QTreeWidgetItem*) {
                         callback(callback_id);
                     });
}

extern "C" void qt_tree_widget_on_item_double_clicked(qt_tree_widget_t t,
                                                        qt_callback_void callback,
                                                        long callback_id) {
    QObject::connect(static_cast<QTreeWidget*>(t),
                     &QTreeWidget::itemDoubleClicked,
                     [callback, callback_id](QTreeWidgetItem*, int) {
                         callback(callback_id);
                     });
}

extern "C" void qt_tree_widget_on_item_expanded(qt_tree_widget_t t,
                                                  qt_callback_void callback,
                                                  long callback_id) {
    QObject::connect(static_cast<QTreeWidget*>(t),
                     &QTreeWidget::itemExpanded,
                     [callback, callback_id](QTreeWidgetItem*) {
                         callback(callback_id);
                     });
}

extern "C" void qt_tree_widget_on_item_collapsed(qt_tree_widget_t t,
                                                   qt_callback_void callback,
                                                   long callback_id) {
    QObject::connect(static_cast<QTreeWidget*>(t),
                     &QTreeWidget::itemCollapsed,
                     [callback, callback_id](QTreeWidgetItem*) {
                         callback(callback_id);
                     });
}

// ============================================================
// Tree Widget Item
// ============================================================

extern "C" qt_tree_item_t qt_tree_item_create(const char* text) {
    return new QTreeWidgetItem(QStringList(QString::fromUtf8(text)));
}

extern "C" void qt_tree_item_set_text(qt_tree_item_t item, int col,
                                       const char* text) {
    static_cast<QTreeWidgetItem*>(item)->setText(col, QString::fromUtf8(text));
}

extern "C" const char* qt_tree_item_text(qt_tree_item_t item, int col) {
    s_return_buf = static_cast<QTreeWidgetItem*>(item)->text(col).toUtf8().toStdString();
    return s_return_buf.c_str();
}

extern "C" void qt_tree_item_add_child(qt_tree_item_t parent,
                                        qt_tree_item_t child) {
    static_cast<QTreeWidgetItem*>(parent)->addChild(
        static_cast<QTreeWidgetItem*>(child));
}

extern "C" int qt_tree_item_child_count(qt_tree_item_t item) {
    return static_cast<QTreeWidgetItem*>(item)->childCount();
}

extern "C" qt_tree_item_t qt_tree_item_child(qt_tree_item_t item, int index) {
    return static_cast<QTreeWidgetItem*>(item)->child(index);
}

extern "C" qt_tree_item_t qt_tree_item_parent(qt_tree_item_t item) {
    return static_cast<QTreeWidgetItem*>(item)->parent();
}

extern "C" void qt_tree_item_set_expanded(qt_tree_item_t item, int expanded) {
    static_cast<QTreeWidgetItem*>(item)->setExpanded(expanded != 0);
}

extern "C" int qt_tree_item_is_expanded(qt_tree_item_t item) {
    return static_cast<QTreeWidgetItem*>(item)->isExpanded() ? 1 : 0;
}

// ============================================================
// App-wide Style Sheet
// ============================================================

extern "C" void qt_application_set_style_sheet(qt_application_t app,
                                                const char* css) {
    static_cast<QApplication*>(app)->setStyleSheet(QString::fromUtf8(css));
}

// ============================================================
// Window State Management
// ============================================================

extern "C" void qt_widget_show_minimized(qt_widget_t w) {
    static_cast<QWidget*>(w)->showMinimized();
}

extern "C" void qt_widget_show_maximized(qt_widget_t w) {
    static_cast<QWidget*>(w)->showMaximized();
}

extern "C" void qt_widget_show_fullscreen(qt_widget_t w) {
    static_cast<QWidget*>(w)->showFullScreen();
}

extern "C" void qt_widget_show_normal(qt_widget_t w) {
    static_cast<QWidget*>(w)->showNormal();
}

extern "C" int qt_widget_window_state(qt_widget_t w) {
    return static_cast<int>(static_cast<QWidget*>(w)->windowState());
}

extern "C" void qt_widget_move(qt_widget_t w, int x, int y) {
    static_cast<QWidget*>(w)->move(x, y);
}

extern "C" int qt_widget_x(qt_widget_t w) {
    return static_cast<QWidget*>(w)->x();
}

extern "C" int qt_widget_y(qt_widget_t w) {
    return static_cast<QWidget*>(w)->y();
}

extern "C" int qt_widget_width(qt_widget_t w) {
    return static_cast<QWidget*>(w)->width();
}

extern "C" int qt_widget_height(qt_widget_t w) {
    return static_cast<QWidget*>(w)->height();
}

// ============================================================
// Scroll Area
// ============================================================

extern "C" qt_scroll_area_t qt_scroll_area_create(qt_widget_t parent) {
    return new QScrollArea(static_cast<QWidget*>(parent));
}

extern "C" void qt_scroll_area_set_widget(qt_scroll_area_t s, qt_widget_t w) {
    static_cast<QScrollArea*>(s)->setWidget(static_cast<QWidget*>(w));
}

extern "C" void qt_scroll_area_set_widget_resizable(qt_scroll_area_t s,
                                                     int resizable) {
    static_cast<QScrollArea*>(s)->setWidgetResizable(resizable != 0);
}

extern "C" void qt_scroll_area_set_horizontal_scrollbar_policy(
    qt_scroll_area_t s, int policy) {
    static_cast<QScrollArea*>(s)->setHorizontalScrollBarPolicy(
        static_cast<Qt::ScrollBarPolicy>(policy));
}

extern "C" void qt_scroll_area_set_vertical_scrollbar_policy(
    qt_scroll_area_t s, int policy) {
    static_cast<QScrollArea*>(s)->setVerticalScrollBarPolicy(
        static_cast<Qt::ScrollBarPolicy>(policy));
}

// ============================================================
// Splitter
// ============================================================

extern "C" qt_splitter_t qt_splitter_create(int orientation,
                                             qt_widget_t parent) {
    return new QSplitter(static_cast<Qt::Orientation>(orientation),
                         static_cast<QWidget*>(parent));
}

extern "C" void qt_splitter_add_widget(qt_splitter_t s, qt_widget_t w) {
    static_cast<QSplitter*>(s)->addWidget(static_cast<QWidget*>(w));
}

extern "C" int qt_splitter_count(qt_splitter_t s) {
    return static_cast<QSplitter*>(s)->count();
}

extern "C" void qt_splitter_set_sizes_2(qt_splitter_t s, int a, int b) {
    static_cast<QSplitter*>(s)->setSizes({a, b});
}

extern "C" void qt_splitter_set_sizes_3(qt_splitter_t s, int a, int b, int c) {
    static_cast<QSplitter*>(s)->setSizes({a, b, c});
}

extern "C" int qt_splitter_size_at(qt_splitter_t s, int index) {
    QList<int> sizes = static_cast<QSplitter*>(s)->sizes();
    if (index >= 0 && index < sizes.size())
        return sizes[index];
    return 0;
}

extern "C" void qt_splitter_set_stretch_factor(qt_splitter_t s, int index,
                                                int stretch) {
    static_cast<QSplitter*>(s)->setStretchFactor(index, stretch);
}

extern "C" void qt_splitter_set_handle_width(qt_splitter_t s, int width) {
    static_cast<QSplitter*>(s)->setHandleWidth(width);
}

extern "C" void qt_splitter_set_collapsible(qt_splitter_t s, int index,
                                             int collapsible) {
    static_cast<QSplitter*>(s)->setCollapsible(index, collapsible != 0);
}

extern "C" int qt_splitter_is_collapsible(qt_splitter_t s, int index) {
    return static_cast<QSplitter*>(s)->isCollapsible(index) ? 1 : 0;
}

// ============================================================
// Keyboard Events
// ============================================================

extern "C" void qt_widget_install_key_handler(qt_widget_t w,
                                               qt_callback_void callback,
                                               long callback_id) {
    auto* widget = static_cast<QWidget*>(w);
    auto* filter = new KeyPressFilter(widget, callback, callback_id);
    widget->installEventFilter(filter);
}

extern "C" int qt_last_key_code(void) {
    return s_last_key_code;
}

extern "C" int qt_last_key_modifiers(void) {
    return s_last_key_modifiers;
}

extern "C" const char* qt_last_key_text(void) {
    return s_last_key_text.c_str();
}

// ============================================================
// Pixmap
// ============================================================

extern "C" qt_pixmap_t qt_pixmap_load(const char* path) {
    auto* pm = new QPixmap(QString::fromUtf8(path));
    return pm;
}

extern "C" int qt_pixmap_width(qt_pixmap_t p) {
    return static_cast<QPixmap*>(p)->width();
}

extern "C" int qt_pixmap_height(qt_pixmap_t p) {
    return static_cast<QPixmap*>(p)->height();
}

extern "C" int qt_pixmap_is_null(qt_pixmap_t p) {
    return static_cast<QPixmap*>(p)->isNull() ? 1 : 0;
}

extern "C" qt_pixmap_t qt_pixmap_scaled(qt_pixmap_t p, int w, int h) {
    auto* scaled = new QPixmap(
        static_cast<QPixmap*>(p)->scaled(w, h, Qt::KeepAspectRatio,
                                          Qt::SmoothTransformation));
    return scaled;
}

extern "C" void qt_pixmap_destroy(qt_pixmap_t p) {
    delete static_cast<QPixmap*>(p);
}

extern "C" void qt_label_set_pixmap(qt_label_t label, qt_pixmap_t pixmap) {
    static_cast<QLabel*>(label)->setPixmap(*static_cast<QPixmap*>(pixmap));
}

// ============================================================
// Icon
// ============================================================

extern "C" qt_icon_t qt_icon_create(const char* path) {
    return new QIcon(QString::fromUtf8(path));
}

extern "C" qt_icon_t qt_icon_create_from_pixmap(qt_pixmap_t pixmap) {
    return new QIcon(*static_cast<QPixmap*>(pixmap));
}

extern "C" int qt_icon_is_null(qt_icon_t icon) {
    return static_cast<QIcon*>(icon)->isNull() ? 1 : 0;
}

extern "C" void qt_icon_destroy(qt_icon_t icon) {
    delete static_cast<QIcon*>(icon);
}

extern "C" void qt_push_button_set_icon(qt_push_button_t button,
                                         qt_icon_t icon) {
    static_cast<QPushButton*>(button)->setIcon(*static_cast<QIcon*>(icon));
}

extern "C" void qt_action_set_icon(qt_action_t action, qt_icon_t icon) {
    static_cast<QAction*>(action)->setIcon(*static_cast<QIcon*>(icon));
}

extern "C" void qt_widget_set_window_icon(qt_widget_t widget,
                                           qt_icon_t icon) {
    static_cast<QWidget*>(widget)->setWindowIcon(*static_cast<QIcon*>(icon));
}

// ============================================================
// Radio Button
// ============================================================

extern "C" qt_radio_button_t qt_radio_button_create(const char* text,
                                                      qt_widget_t parent) {
    return new QRadioButton(QString::fromUtf8(text),
                            static_cast<QWidget*>(parent));
}

extern "C" void qt_radio_button_set_text(qt_radio_button_t r,
                                          const char* text) {
    static_cast<QRadioButton*>(r)->setText(QString::fromUtf8(text));
}

extern "C" const char* qt_radio_button_text(qt_radio_button_t r) {
    s_return_buf = static_cast<QRadioButton*>(r)->text().toUtf8().toStdString();
    return s_return_buf.c_str();
}

extern "C" void qt_radio_button_set_checked(qt_radio_button_t r,
                                             int checked) {
    static_cast<QRadioButton*>(r)->setChecked(checked != 0);
}

extern "C" int qt_radio_button_is_checked(qt_radio_button_t r) {
    return static_cast<QRadioButton*>(r)->isChecked() ? 1 : 0;
}

extern "C" void qt_radio_button_on_toggled(qt_radio_button_t r,
                                            qt_callback_bool callback,
                                            long callback_id) {
    QObject::connect(static_cast<QRadioButton*>(r), &QRadioButton::toggled,
                     [callback, callback_id](bool checked) {
                         callback(callback_id, checked ? 1 : 0);
                     });
}

// ============================================================
// Button Group
// ============================================================

extern "C" qt_button_group_t qt_button_group_create(void) {
    return new QButtonGroup();
}

extern "C" void qt_button_group_add_button(qt_button_group_t bg,
                                            qt_widget_t button, int id) {
    static_cast<QButtonGroup*>(bg)->addButton(
        static_cast<QAbstractButton*>(button), id);
}

extern "C" void qt_button_group_remove_button(qt_button_group_t bg,
                                               qt_widget_t button) {
    static_cast<QButtonGroup*>(bg)->removeButton(
        static_cast<QAbstractButton*>(button));
}

extern "C" int qt_button_group_checked_id(qt_button_group_t bg) {
    return static_cast<QButtonGroup*>(bg)->checkedId();
}

extern "C" void qt_button_group_set_exclusive(qt_button_group_t bg,
                                               int exclusive) {
    static_cast<QButtonGroup*>(bg)->setExclusive(exclusive != 0);
}

extern "C" int qt_button_group_is_exclusive(qt_button_group_t bg) {
    return static_cast<QButtonGroup*>(bg)->exclusive() ? 1 : 0;
}

extern "C" void qt_button_group_on_id_clicked(qt_button_group_t bg,
                                               qt_callback_int callback,
                                               long callback_id) {
    QObject::connect(static_cast<QButtonGroup*>(bg),
                     &QButtonGroup::idClicked,
                     [callback, callback_id](int id) {
                         callback(callback_id, id);
                     });
}

extern "C" void qt_button_group_destroy(qt_button_group_t bg) {
    delete static_cast<QButtonGroup*>(bg);
}

// ============================================================
// Group Box
// ============================================================

extern "C" qt_group_box_t qt_group_box_create(const char* title,
                                               qt_widget_t parent) {
    return new QGroupBox(QString::fromUtf8(title),
                         static_cast<QWidget*>(parent));
}

extern "C" void qt_group_box_set_title(qt_group_box_t gb,
                                        const char* title) {
    static_cast<QGroupBox*>(gb)->setTitle(QString::fromUtf8(title));
}

extern "C" const char* qt_group_box_title(qt_group_box_t gb) {
    s_return_buf = static_cast<QGroupBox*>(gb)->title().toUtf8().toStdString();
    return s_return_buf.c_str();
}

extern "C" void qt_group_box_set_checkable(qt_group_box_t gb,
                                            int checkable) {
    static_cast<QGroupBox*>(gb)->setCheckable(checkable != 0);
}

extern "C" int qt_group_box_is_checkable(qt_group_box_t gb) {
    return static_cast<QGroupBox*>(gb)->isCheckable() ? 1 : 0;
}

extern "C" void qt_group_box_set_checked(qt_group_box_t gb, int checked) {
    static_cast<QGroupBox*>(gb)->setChecked(checked != 0);
}

extern "C" int qt_group_box_is_checked(qt_group_box_t gb) {
    return static_cast<QGroupBox*>(gb)->isChecked() ? 1 : 0;
}

extern "C" void qt_group_box_on_toggled(qt_group_box_t gb,
                                         qt_callback_bool callback,
                                         long callback_id) {
    QObject::connect(static_cast<QGroupBox*>(gb), &QGroupBox::toggled,
                     [callback, callback_id](bool checked) {
                         callback(callback_id, checked ? 1 : 0);
                     });
}

// ============================================================
// Phase 8a: Font
// ============================================================

extern "C" qt_font_t qt_font_create(const char* family, int point_size) {
    return new QFont(QString::fromUtf8(family), point_size);
}

extern "C" const char* qt_font_family(qt_font_t f) {
    s_return_buf = static_cast<QFont*>(f)->family().toUtf8().toStdString();
    return s_return_buf.c_str();
}

extern "C" int qt_font_point_size(qt_font_t f) {
    return static_cast<QFont*>(f)->pointSize();
}

extern "C" void qt_font_set_bold(qt_font_t f, int bold) {
    static_cast<QFont*>(f)->setBold(bold != 0);
}

extern "C" int qt_font_is_bold(qt_font_t f) {
    return static_cast<QFont*>(f)->bold() ? 1 : 0;
}

extern "C" void qt_font_set_italic(qt_font_t f, int italic) {
    static_cast<QFont*>(f)->setItalic(italic != 0);
}

extern "C" int qt_font_is_italic(qt_font_t f) {
    return static_cast<QFont*>(f)->italic() ? 1 : 0;
}

extern "C" void qt_font_destroy(qt_font_t f) {
    delete static_cast<QFont*>(f);
}

extern "C" void qt_widget_set_font(qt_widget_t w, qt_font_t f) {
    static_cast<QWidget*>(w)->setFont(*static_cast<QFont*>(f));
}

extern "C" qt_font_t qt_widget_font(qt_widget_t w) {
    return new QFont(static_cast<QWidget*>(w)->font());
}

// ============================================================
// Phase 8a: Color
// ============================================================

extern "C" qt_color_t qt_color_create_rgb(int r, int g, int b, int a) {
    return new QColor(r, g, b, a);
}

extern "C" qt_color_t qt_color_create_name(const char* name) {
    return new QColor(QString::fromUtf8(name));
}

extern "C" int qt_color_red(qt_color_t c) {
    return static_cast<QColor*>(c)->red();
}

extern "C" int qt_color_green(qt_color_t c) {
    return static_cast<QColor*>(c)->green();
}

extern "C" int qt_color_blue(qt_color_t c) {
    return static_cast<QColor*>(c)->blue();
}

extern "C" int qt_color_alpha(qt_color_t c) {
    return static_cast<QColor*>(c)->alpha();
}

extern "C" const char* qt_color_name(qt_color_t c) {
    s_return_buf = static_cast<QColor*>(c)->name().toUtf8().toStdString();
    return s_return_buf.c_str();
}

extern "C" int qt_color_is_valid(qt_color_t c) {
    return static_cast<QColor*>(c)->isValid() ? 1 : 0;
}

extern "C" void qt_color_destroy(qt_color_t c) {
    delete static_cast<QColor*>(c);
}

// ============================================================
// Phase 8a: Font Dialog
// ============================================================

extern "C" qt_font_t qt_font_dialog_get_font(qt_widget_t parent) {
    bool ok = false;
    QFont font = QFontDialog::getFont(&ok, QFont(),
                                       static_cast<QWidget*>(parent));
    if (ok) {
        return new QFont(font);
    }
    return nullptr;
}

// ============================================================
// Phase 8a: Color Dialog
// ============================================================

extern "C" qt_color_t qt_color_dialog_get_color(const char* initial,
                                                  qt_widget_t parent) {
    QColor init(QString::fromUtf8(initial));
    QColor color = QColorDialog::getColor(init,
                                           static_cast<QWidget*>(parent));
    return new QColor(color);
}

// ============================================================
// Phase 8b: Stacked Widget
// ============================================================

extern "C" qt_stacked_widget_t qt_stacked_widget_create(qt_widget_t parent) {
    return new QStackedWidget(static_cast<QWidget*>(parent));
}

extern "C" int qt_stacked_widget_add_widget(qt_stacked_widget_t sw,
                                             qt_widget_t w) {
    return static_cast<QStackedWidget*>(sw)->addWidget(
        static_cast<QWidget*>(w));
}

extern "C" void qt_stacked_widget_set_current_index(qt_stacked_widget_t sw,
                                                     int idx) {
    static_cast<QStackedWidget*>(sw)->setCurrentIndex(idx);
}

extern "C" int qt_stacked_widget_current_index(qt_stacked_widget_t sw) {
    return static_cast<QStackedWidget*>(sw)->currentIndex();
}

extern "C" int qt_stacked_widget_count(qt_stacked_widget_t sw) {
    return static_cast<QStackedWidget*>(sw)->count();
}

extern "C" void qt_stacked_widget_on_current_changed(qt_stacked_widget_t sw,
                                                      qt_callback_int callback,
                                                      long callback_id) {
    QObject::connect(static_cast<QStackedWidget*>(sw),
                     &QStackedWidget::currentChanged,
                     [callback, callback_id](int index) {
                         callback(callback_id, index);
                     });
}

// ============================================================
// Phase 8b: Dock Widget
// ============================================================

extern "C" qt_dock_widget_t qt_dock_widget_create(const char* title,
                                                    qt_widget_t parent) {
    return new QDockWidget(QString::fromUtf8(title),
                           static_cast<QWidget*>(parent));
}

extern "C" void qt_dock_widget_set_widget(qt_dock_widget_t dw,
                                           qt_widget_t w) {
    static_cast<QDockWidget*>(dw)->setWidget(static_cast<QWidget*>(w));
}

extern "C" qt_widget_t qt_dock_widget_widget(qt_dock_widget_t dw) {
    return static_cast<QDockWidget*>(dw)->widget();
}

extern "C" void qt_dock_widget_set_title(qt_dock_widget_t dw,
                                          const char* title) {
    static_cast<QDockWidget*>(dw)->setWindowTitle(QString::fromUtf8(title));
}

extern "C" const char* qt_dock_widget_title(qt_dock_widget_t dw) {
    s_return_buf = static_cast<QDockWidget*>(dw)->windowTitle()
                       .toUtf8().toStdString();
    return s_return_buf.c_str();
}

extern "C" void qt_dock_widget_set_floating(qt_dock_widget_t dw,
                                             int floating) {
    static_cast<QDockWidget*>(dw)->setFloating(floating != 0);
}

extern "C" int qt_dock_widget_is_floating(qt_dock_widget_t dw) {
    return static_cast<QDockWidget*>(dw)->isFloating() ? 1 : 0;
}

extern "C" void qt_main_window_add_dock_widget(qt_main_window_t mw, int area,
                                                qt_dock_widget_t dw) {
    static_cast<QMainWindow*>(mw)->addDockWidget(
        static_cast<Qt::DockWidgetArea>(area),
        static_cast<QDockWidget*>(dw));
}

// ============================================================
// Phase 8c: System Tray Icon
// ============================================================

extern "C" qt_tray_icon_t qt_system_tray_icon_create(qt_icon_t icon,
                                                      qt_widget_t parent) {
    return new QSystemTrayIcon(*static_cast<QIcon*>(icon),
                               static_cast<QWidget*>(parent));
}

extern "C" void qt_system_tray_icon_set_tooltip(qt_tray_icon_t ti,
                                                 const char* text) {
    static_cast<QSystemTrayIcon*>(ti)->setToolTip(QString::fromUtf8(text));
}

extern "C" void qt_system_tray_icon_set_icon(qt_tray_icon_t ti,
                                              qt_icon_t icon) {
    static_cast<QSystemTrayIcon*>(ti)->setIcon(*static_cast<QIcon*>(icon));
}

extern "C" void qt_system_tray_icon_show(qt_tray_icon_t ti) {
    static_cast<QSystemTrayIcon*>(ti)->show();
}

extern "C" void qt_system_tray_icon_hide(qt_tray_icon_t ti) {
    static_cast<QSystemTrayIcon*>(ti)->hide();
}

extern "C" void qt_system_tray_icon_show_message(qt_tray_icon_t ti,
                                                  const char* title,
                                                  const char* msg,
                                                  int icon_type,
                                                  int msecs) {
    static_cast<QSystemTrayIcon*>(ti)->showMessage(
        QString::fromUtf8(title),
        QString::fromUtf8(msg),
        static_cast<QSystemTrayIcon::MessageIcon>(icon_type),
        msecs);
}

extern "C" void qt_system_tray_icon_set_context_menu(qt_tray_icon_t ti,
                                                      qt_menu_t menu) {
    static_cast<QSystemTrayIcon*>(ti)->setContextMenu(
        static_cast<QMenu*>(menu));
}

extern "C" void qt_system_tray_icon_on_activated(qt_tray_icon_t ti,
                                                  qt_callback_int callback,
                                                  long callback_id) {
    QObject::connect(static_cast<QSystemTrayIcon*>(ti),
                     &QSystemTrayIcon::activated,
                     [callback, callback_id](QSystemTrayIcon::ActivationReason reason) {
                         callback(callback_id, static_cast<int>(reason));
                     });
}

extern "C" int qt_system_tray_icon_is_available(void) {
    return QSystemTrayIcon::isSystemTrayAvailable() ? 1 : 0;
}

extern "C" void qt_system_tray_icon_destroy(qt_tray_icon_t ti) {
    delete static_cast<QSystemTrayIcon*>(ti);
}

// ============================================================
// Phase 8d: QPainter
// ============================================================

extern "C" qt_pixmap_t qt_pixmap_create_blank(int w, int h) {
    return new QPixmap(w, h);
}

extern "C" void qt_pixmap_fill(qt_pixmap_t pm, int r, int g, int b, int a) {
    static_cast<QPixmap*>(pm)->fill(QColor(r, g, b, a));
}

extern "C" qt_painter_t qt_painter_create(qt_pixmap_t pixmap) {
    return new QPainter(static_cast<QPixmap*>(pixmap));
}

extern "C" void qt_painter_end(qt_painter_t p) {
    static_cast<QPainter*>(p)->end();
}

extern "C" void qt_painter_destroy(qt_painter_t p) {
    QPainter* painter = static_cast<QPainter*>(p);
    if (painter->isActive()) {
        painter->end();
    }
    delete painter;
}

extern "C" void qt_painter_set_pen_color(qt_painter_t p,
                                          int r, int g, int b, int a) {
    QPen pen = static_cast<QPainter*>(p)->pen();
    pen.setColor(QColor(r, g, b, a));
    static_cast<QPainter*>(p)->setPen(pen);
}

extern "C" void qt_painter_set_pen_width(qt_painter_t p, int width) {
    QPen pen = static_cast<QPainter*>(p)->pen();
    pen.setWidth(width);
    static_cast<QPainter*>(p)->setPen(pen);
}

extern "C" void qt_painter_set_brush_color(qt_painter_t p,
                                            int r, int g, int b, int a) {
    static_cast<QPainter*>(p)->setBrush(QBrush(QColor(r, g, b, a)));
}

extern "C" void qt_painter_set_font(qt_painter_t p, qt_font_t font) {
    static_cast<QPainter*>(p)->setFont(*static_cast<QFont*>(font));
}

extern "C" void qt_painter_set_antialiasing(qt_painter_t p, int enabled) {
    static_cast<QPainter*>(p)->setRenderHint(QPainter::Antialiasing,
                                              enabled != 0);
}

extern "C" void qt_painter_draw_line(qt_painter_t p,
                                      int x1, int y1, int x2, int y2) {
    static_cast<QPainter*>(p)->drawLine(x1, y1, x2, y2);
}

extern "C" void qt_painter_draw_rect(qt_painter_t p,
                                      int x, int y, int w, int h) {
    static_cast<QPainter*>(p)->drawRect(x, y, w, h);
}

extern "C" void qt_painter_fill_rect(qt_painter_t p,
                                      int x, int y, int w, int h,
                                      int r, int g, int b, int a) {
    static_cast<QPainter*>(p)->fillRect(x, y, w, h, QColor(r, g, b, a));
}

extern "C" void qt_painter_draw_ellipse(qt_painter_t p,
                                         int x, int y, int w, int h) {
    static_cast<QPainter*>(p)->drawEllipse(x, y, w, h);
}

extern "C" void qt_painter_draw_text(qt_painter_t p,
                                      int x, int y, const char* text) {
    static_cast<QPainter*>(p)->drawText(x, y, QString::fromUtf8(text));
}

extern "C" void qt_painter_draw_text_rect(qt_painter_t p,
                                           int x, int y, int w, int h,
                                           int flags, const char* text) {
    static_cast<QPainter*>(p)->drawText(QRect(x, y, w, h), flags,
                                         QString::fromUtf8(text));
}

extern "C" void qt_painter_draw_pixmap(qt_painter_t p,
                                        int x, int y, qt_pixmap_t pixmap) {
    static_cast<QPainter*>(p)->drawPixmap(x, y,
                                           *static_cast<QPixmap*>(pixmap));
}

extern "C" void qt_painter_draw_point(qt_painter_t p, int x, int y) {
    static_cast<QPainter*>(p)->drawPoint(x, y);
}

extern "C" void qt_painter_draw_arc(qt_painter_t p,
                                     int x, int y, int w, int h,
                                     int start_angle, int span_angle) {
    static_cast<QPainter*>(p)->drawArc(x, y, w, h, start_angle, span_angle);
}

extern "C" void qt_painter_save(qt_painter_t p) {
    static_cast<QPainter*>(p)->save();
}

extern "C" void qt_painter_restore(qt_painter_t p) {
    static_cast<QPainter*>(p)->restore();
}

extern "C" void qt_painter_translate(qt_painter_t p, int dx, int dy) {
    static_cast<QPainter*>(p)->translate(dx, dy);
}

extern "C" void qt_painter_rotate(qt_painter_t p, double angle) {
    static_cast<QPainter*>(p)->rotate(angle);
}

extern "C" void qt_painter_scale(qt_painter_t p, double sx, double sy) {
    static_cast<QPainter*>(p)->scale(sx, sy);
}

// ============================================================
// Phase 8e: Drag and Drop
// ============================================================

// DropFilter — QObject subclass that intercepts DragEnter and Drop events
// Same pattern as KeyPressFilter from Phase 6
class DropFilter : public QObject {
public:
    qt_callback_string m_callback;
    long m_callback_id;
    std::string m_last_text;

    DropFilter(QWidget* target, qt_callback_string callback, long callback_id)
        : QObject(target), m_callback(callback), m_callback_id(callback_id) {
        target->setAcceptDrops(true);
        target->installEventFilter(this);
    }

    bool eventFilter(QObject* obj, QEvent* event) override {
        if (event->type() == QEvent::DragEnter) {
            auto* de = static_cast<QDragEnterEvent*>(event);
            if (de->mimeData()->hasText()) {
                de->acceptProposedAction();
                return true;
            }
        } else if (event->type() == QEvent::Drop) {
            auto* de = static_cast<QDropEvent*>(event);
            if (de->mimeData()->hasText()) {
                m_last_text = de->mimeData()->text().toUtf8().toStdString();
                de->acceptProposedAction();
                m_callback(m_callback_id, m_last_text.c_str());
                return true;
            }
        }
        return QObject::eventFilter(obj, event);
    }
};

extern "C" void qt_widget_set_accept_drops(qt_widget_t w, int accept) {
    static_cast<QWidget*>(w)->setAcceptDrops(accept != 0);
}

extern "C" qt_drop_filter_t qt_drop_filter_install(qt_widget_t widget,
                                                    qt_callback_string callback,
                                                    long callback_id) {
    return new DropFilter(static_cast<QWidget*>(widget), callback, callback_id);
}

extern "C" const char* qt_drop_filter_last_text(qt_drop_filter_t df) {
    return static_cast<DropFilter*>(df)->m_last_text.c_str();
}

extern "C" void qt_drop_filter_destroy(qt_drop_filter_t df) {
    delete static_cast<DropFilter*>(df);
}

extern "C" void qt_drag_text(qt_widget_t source, const char* text) {
    QDrag* drag = new QDrag(static_cast<QWidget*>(source));
    QMimeData* mimeData = new QMimeData;
    mimeData->setText(QString::fromUtf8(text));
    drag->setMimeData(mimeData);
    drag->exec(Qt::CopyAction);
}

// ============================================================
// Phase 9: Double Spin Box
// ============================================================

extern "C" qt_double_spin_box_t qt_double_spin_box_create(qt_widget_t parent) {
    return new QDoubleSpinBox(static_cast<QWidget*>(parent));
}

extern "C" void qt_double_spin_box_set_value(qt_double_spin_box_t s, double value) {
    static_cast<QDoubleSpinBox*>(s)->setValue(value);
}

extern "C" double qt_double_spin_box_value(qt_double_spin_box_t s) {
    return static_cast<QDoubleSpinBox*>(s)->value();
}

extern "C" void qt_double_spin_box_set_range(qt_double_spin_box_t s,
                                              double minimum, double maximum) {
    static_cast<QDoubleSpinBox*>(s)->setRange(minimum, maximum);
}

extern "C" void qt_double_spin_box_set_single_step(qt_double_spin_box_t s,
                                                     double step) {
    static_cast<QDoubleSpinBox*>(s)->setSingleStep(step);
}

extern "C" void qt_double_spin_box_set_decimals(qt_double_spin_box_t s,
                                                  int decimals) {
    static_cast<QDoubleSpinBox*>(s)->setDecimals(decimals);
}

extern "C" int qt_double_spin_box_decimals(qt_double_spin_box_t s) {
    return static_cast<QDoubleSpinBox*>(s)->decimals();
}

extern "C" void qt_double_spin_box_set_prefix(qt_double_spin_box_t s,
                                               const char* prefix) {
    static_cast<QDoubleSpinBox*>(s)->setPrefix(QString::fromUtf8(prefix));
}

extern "C" void qt_double_spin_box_set_suffix(qt_double_spin_box_t s,
                                               const char* suffix) {
    static_cast<QDoubleSpinBox*>(s)->setSuffix(QString::fromUtf8(suffix));
}

extern "C" void qt_double_spin_box_on_value_changed(qt_double_spin_box_t s,
                                                      qt_callback_string callback,
                                                      long callback_id) {
    QObject::connect(static_cast<QDoubleSpinBox*>(s),
                     QOverload<double>::of(&QDoubleSpinBox::valueChanged),
                     [callback, callback_id](double value) {
                         char buf[64];
                         snprintf(buf, sizeof(buf), "%.17g", value);
                         callback(callback_id, buf);
                     });
}

// ============================================================
// Phase 9: Date Edit
// ============================================================

extern "C" qt_date_edit_t qt_date_edit_create(qt_widget_t parent) {
    return new QDateEdit(static_cast<QWidget*>(parent));
}

extern "C" void qt_date_edit_set_date(qt_date_edit_t d,
                                       int year, int month, int day) {
    static_cast<QDateEdit*>(d)->setDate(QDate(year, month, day));
}

extern "C" int qt_date_edit_year(qt_date_edit_t d) {
    return static_cast<QDateEdit*>(d)->date().year();
}

extern "C" int qt_date_edit_month(qt_date_edit_t d) {
    return static_cast<QDateEdit*>(d)->date().month();
}

extern "C" int qt_date_edit_day(qt_date_edit_t d) {
    return static_cast<QDateEdit*>(d)->date().day();
}

extern "C" const char* qt_date_edit_date_string(qt_date_edit_t d) {
    s_return_buf = static_cast<QDateEdit*>(d)->date()
                       .toString(Qt::ISODate).toUtf8().toStdString();
    return s_return_buf.c_str();
}

extern "C" void qt_date_edit_set_minimum_date(qt_date_edit_t d,
                                               int year, int month, int day) {
    static_cast<QDateEdit*>(d)->setMinimumDate(QDate(year, month, day));
}

extern "C" void qt_date_edit_set_maximum_date(qt_date_edit_t d,
                                               int year, int month, int day) {
    static_cast<QDateEdit*>(d)->setMaximumDate(QDate(year, month, day));
}

extern "C" void qt_date_edit_set_calendar_popup(qt_date_edit_t d, int enabled) {
    static_cast<QDateEdit*>(d)->setCalendarPopup(enabled != 0);
}

extern "C" void qt_date_edit_set_display_format(qt_date_edit_t d,
                                                 const char* format) {
    static_cast<QDateEdit*>(d)->setDisplayFormat(QString::fromUtf8(format));
}

extern "C" void qt_date_edit_on_date_changed(qt_date_edit_t d,
                                              qt_callback_string callback,
                                              long callback_id) {
    QObject::connect(static_cast<QDateEdit*>(d),
                     &QDateEdit::dateChanged,
                     [callback, callback_id](const QDate& date) {
                         std::string iso = date.toString(Qt::ISODate)
                                               .toUtf8().toStdString();
                         callback(callback_id, iso.c_str());
                     });
}

// ============================================================
// Phase 9: Time Edit
// ============================================================

extern "C" qt_time_edit_t qt_time_edit_create(qt_widget_t parent) {
    return new QTimeEdit(static_cast<QWidget*>(parent));
}

extern "C" void qt_time_edit_set_time(qt_time_edit_t t,
                                       int hour, int minute, int second) {
    static_cast<QTimeEdit*>(t)->setTime(QTime(hour, minute, second));
}

extern "C" int qt_time_edit_hour(qt_time_edit_t t) {
    return static_cast<QTimeEdit*>(t)->time().hour();
}

extern "C" int qt_time_edit_minute(qt_time_edit_t t) {
    return static_cast<QTimeEdit*>(t)->time().minute();
}

extern "C" int qt_time_edit_second(qt_time_edit_t t) {
    return static_cast<QTimeEdit*>(t)->time().second();
}

extern "C" const char* qt_time_edit_time_string(qt_time_edit_t t) {
    s_return_buf = static_cast<QTimeEdit*>(t)->time()
                       .toString(Qt::ISODate).toUtf8().toStdString();
    return s_return_buf.c_str();
}

extern "C" void qt_time_edit_set_display_format(qt_time_edit_t t,
                                                 const char* format) {
    static_cast<QTimeEdit*>(t)->setDisplayFormat(QString::fromUtf8(format));
}

extern "C" void qt_time_edit_on_time_changed(qt_time_edit_t t,
                                              qt_callback_string callback,
                                              long callback_id) {
    QObject::connect(static_cast<QTimeEdit*>(t),
                     &QTimeEdit::timeChanged,
                     [callback, callback_id](const QTime& time) {
                         std::string iso = time.toString(Qt::ISODate)
                                               .toUtf8().toStdString();
                         callback(callback_id, iso.c_str());
                     });
}

// ============================================================
// Phase 9: Frame
// ============================================================

extern "C" qt_frame_t qt_frame_create(qt_widget_t parent) {
    return new QFrame(static_cast<QWidget*>(parent));
}

extern "C" void qt_frame_set_frame_shape(qt_frame_t f, int shape) {
    static_cast<QFrame*>(f)->setFrameShape(
        static_cast<QFrame::Shape>(shape));
}

extern "C" int qt_frame_frame_shape(qt_frame_t f) {
    return static_cast<int>(static_cast<QFrame*>(f)->frameShape());
}

extern "C" void qt_frame_set_frame_shadow(qt_frame_t f, int shadow) {
    static_cast<QFrame*>(f)->setFrameShadow(
        static_cast<QFrame::Shadow>(shadow));
}

extern "C" int qt_frame_frame_shadow(qt_frame_t f) {
    return static_cast<int>(static_cast<QFrame*>(f)->frameShadow());
}

extern "C" void qt_frame_set_line_width(qt_frame_t f, int width) {
    static_cast<QFrame*>(f)->setLineWidth(width);
}

extern "C" int qt_frame_line_width(qt_frame_t f) {
    return static_cast<QFrame*>(f)->lineWidth();
}

extern "C" void qt_frame_set_mid_line_width(qt_frame_t f, int width) {
    static_cast<QFrame*>(f)->setMidLineWidth(width);
}

// ============================================================
// Phase 9: Progress Dialog
// ============================================================

extern "C" qt_progress_dialog_t qt_progress_dialog_create(
    const char* label, const char* cancel_text,
    int minimum, int maximum, qt_widget_t parent) {
    auto* pd = new QProgressDialog(
        QString::fromUtf8(label),
        QString::fromUtf8(cancel_text),
        minimum, maximum,
        static_cast<QWidget*>(parent));
    // Don't auto-show — let the user control visibility
    pd->setMinimumDuration(0);
    pd->reset();
    return pd;
}

extern "C" void qt_progress_dialog_set_value(qt_progress_dialog_t pd, int value) {
    static_cast<QProgressDialog*>(pd)->setValue(value);
}

extern "C" int qt_progress_dialog_value(qt_progress_dialog_t pd) {
    return static_cast<QProgressDialog*>(pd)->value();
}

extern "C" void qt_progress_dialog_set_range(qt_progress_dialog_t pd,
                                              int minimum, int maximum) {
    static_cast<QProgressDialog*>(pd)->setRange(minimum, maximum);
}

extern "C" void qt_progress_dialog_set_label_text(qt_progress_dialog_t pd,
                                                    const char* text) {
    static_cast<QProgressDialog*>(pd)->setLabelText(QString::fromUtf8(text));
}

extern "C" int qt_progress_dialog_was_canceled(qt_progress_dialog_t pd) {
    return static_cast<QProgressDialog*>(pd)->wasCanceled() ? 1 : 0;
}

extern "C" void qt_progress_dialog_set_minimum_duration(qt_progress_dialog_t pd,
                                                         int msecs) {
    static_cast<QProgressDialog*>(pd)->setMinimumDuration(msecs);
}

extern "C" void qt_progress_dialog_set_auto_close(qt_progress_dialog_t pd,
                                                    int enabled) {
    static_cast<QProgressDialog*>(pd)->setAutoClose(enabled != 0);
}

extern "C" void qt_progress_dialog_set_auto_reset(qt_progress_dialog_t pd,
                                                    int enabled) {
    static_cast<QProgressDialog*>(pd)->setAutoReset(enabled != 0);
}

extern "C" void qt_progress_dialog_reset(qt_progress_dialog_t pd) {
    static_cast<QProgressDialog*>(pd)->reset();
}

extern "C" void qt_progress_dialog_on_canceled(qt_progress_dialog_t pd,
                                                qt_callback_void callback,
                                                long callback_id) {
    QObject::connect(static_cast<QProgressDialog*>(pd),
                     &QProgressDialog::canceled,
                     [callback, callback_id]() {
                         callback(callback_id);
                     });
}

// ============================================================
// Phase 9: Input Dialog (static convenience)
// ============================================================

extern "C" const char* qt_input_dialog_get_text(
    qt_widget_t parent, const char* title,
    const char* label, const char* default_text) {
    s_last_input_ok = false;
    s_return_buf = QInputDialog::getText(
        static_cast<QWidget*>(parent),
        QString::fromUtf8(title),
        QString::fromUtf8(label),
        QLineEdit::Normal,
        QString::fromUtf8(default_text),
        &s_last_input_ok).toUtf8().toStdString();
    return s_return_buf.c_str();
}

extern "C" int qt_input_dialog_get_int(
    qt_widget_t parent, const char* title,
    const char* label, int value,
    int min_val, int max_val, int step) {
    s_last_input_ok = false;
    return QInputDialog::getInt(
        static_cast<QWidget*>(parent),
        QString::fromUtf8(title),
        QString::fromUtf8(label),
        value, min_val, max_val, step,
        &s_last_input_ok);
}

extern "C" double qt_input_dialog_get_double(
    qt_widget_t parent, const char* title,
    const char* label, double value,
    double min_val, double max_val, int decimals) {
    s_last_input_ok = false;
    return QInputDialog::getDouble(
        static_cast<QWidget*>(parent),
        QString::fromUtf8(title),
        QString::fromUtf8(label),
        value, min_val, max_val, decimals,
        &s_last_input_ok);
}

extern "C" const char* qt_input_dialog_get_item(
    qt_widget_t parent, const char* title,
    const char* label, const char* items_newline,
    int current, int editable) {
    s_last_input_ok = false;
    QStringList items = QString::fromUtf8(items_newline)
                            .split('\n', Qt::SkipEmptyParts);
    s_return_buf = QInputDialog::getItem(
        static_cast<QWidget*>(parent),
        QString::fromUtf8(title),
        QString::fromUtf8(label),
        items, current, editable != 0,
        &s_last_input_ok).toUtf8().toStdString();
    return s_return_buf.c_str();
}

extern "C" int qt_input_dialog_was_accepted(void) {
    return s_last_input_ok ? 1 : 0;
}

// ============================================================
// Phase 10: Form Layout
// ============================================================

extern "C" qt_layout_t qt_form_layout_create(qt_widget_t parent) {
    return new QFormLayout(static_cast<QWidget*>(parent));
}

extern "C" void qt_form_layout_add_row(qt_layout_t layout, const char* label,
                                        qt_widget_t field) {
    static_cast<QFormLayout*>(layout)->addRow(
        QString::fromUtf8(label), static_cast<QWidget*>(field));
}

extern "C" void qt_form_layout_add_row_widget(qt_layout_t layout,
                                               qt_widget_t label_widget,
                                               qt_widget_t field) {
    static_cast<QFormLayout*>(layout)->addRow(
        static_cast<QWidget*>(label_widget), static_cast<QWidget*>(field));
}

extern "C" void qt_form_layout_add_spanning_widget(qt_layout_t layout,
                                                     qt_widget_t widget) {
    static_cast<QFormLayout*>(layout)->addRow(static_cast<QWidget*>(widget));
}

extern "C" int qt_form_layout_row_count(qt_layout_t layout) {
    return static_cast<QFormLayout*>(layout)->rowCount();
}

// ============================================================
// Phase 10: Shortcut
// ============================================================

extern "C" qt_shortcut_t qt_shortcut_create(const char* key_sequence,
                                             qt_widget_t parent) {
    return new QShortcut(QKeySequence(QString::fromUtf8(key_sequence)),
                         static_cast<QWidget*>(parent));
}

extern "C" void qt_shortcut_set_key(qt_shortcut_t s,
                                     const char* key_sequence) {
    static_cast<QShortcut*>(s)->setKey(
        QKeySequence(QString::fromUtf8(key_sequence)));
}

extern "C" void qt_shortcut_set_enabled(qt_shortcut_t s, int enabled) {
    static_cast<QShortcut*>(s)->setEnabled(enabled != 0);
}

extern "C" int qt_shortcut_is_enabled(qt_shortcut_t s) {
    return static_cast<QShortcut*>(s)->isEnabled() ? 1 : 0;
}

extern "C" void qt_shortcut_on_activated(qt_shortcut_t s,
                                          qt_callback_void callback,
                                          long callback_id) {
    QObject::connect(static_cast<QShortcut*>(s), &QShortcut::activated,
                     [callback, callback_id]() {
                         callback(callback_id);
                     });
}

extern "C" void qt_shortcut_destroy(qt_shortcut_t s) {
    delete static_cast<QShortcut*>(s);
}

// ============================================================
// Phase 10: Text Browser
// ============================================================

extern "C" qt_text_browser_t qt_text_browser_create(qt_widget_t parent) {
    return new QTextBrowser(static_cast<QWidget*>(parent));
}

extern "C" void qt_text_browser_set_html(qt_text_browser_t tb,
                                          const char* html) {
    static_cast<QTextBrowser*>(tb)->setHtml(QString::fromUtf8(html));
}

extern "C" void qt_text_browser_set_plain_text(qt_text_browser_t tb,
                                                const char* text) {
    static_cast<QTextBrowser*>(tb)->setPlainText(QString::fromUtf8(text));
}

extern "C" const char* qt_text_browser_plain_text(qt_text_browser_t tb) {
    s_return_buf = static_cast<QTextBrowser*>(tb)->toPlainText()
                       .toUtf8().toStdString();
    return s_return_buf.c_str();
}

extern "C" void qt_text_browser_set_open_external_links(qt_text_browser_t tb,
                                                          int enabled) {
    static_cast<QTextBrowser*>(tb)->setOpenExternalLinks(enabled != 0);
}

extern "C" void qt_text_browser_set_source(qt_text_browser_t tb,
                                            const char* url) {
    static_cast<QTextBrowser*>(tb)->setSource(
        QUrl(QString::fromUtf8(url)));
}

extern "C" const char* qt_text_browser_source(qt_text_browser_t tb) {
    s_return_buf = static_cast<QTextBrowser*>(tb)->source()
                       .toString().toUtf8().toStdString();
    return s_return_buf.c_str();
}

extern "C" void qt_text_browser_on_anchor_clicked(qt_text_browser_t tb,
                                                    qt_callback_string callback,
                                                    long callback_id) {
    static_cast<QTextBrowser*>(tb)->setOpenLinks(false);
    QObject::connect(static_cast<QTextBrowser*>(tb),
                     &QTextBrowser::anchorClicked,
                     [callback, callback_id](const QUrl& url) {
                         std::string s = url.toString().toUtf8().toStdString();
                         callback(callback_id, s.c_str());
                     });
}

// ============================================================
// Phase 10: Dialog Button Box
// ============================================================

extern "C" qt_button_box_t qt_button_box_create(int standard_buttons,
                                                  qt_widget_t parent) {
    return new QDialogButtonBox(
        static_cast<QDialogButtonBox::StandardButtons>(standard_buttons),
        static_cast<QWidget*>(parent));
}

extern "C" qt_push_button_t qt_button_box_button(qt_button_box_t bb,
                                                   int standard_button) {
    return static_cast<QDialogButtonBox*>(bb)->button(
        static_cast<QDialogButtonBox::StandardButton>(standard_button));
}

extern "C" void qt_button_box_add_button(qt_button_box_t bb,
                                          qt_push_button_t button, int role) {
    static_cast<QDialogButtonBox*>(bb)->addButton(
        static_cast<QPushButton*>(button),
        static_cast<QDialogButtonBox::ButtonRole>(role));
}

extern "C" void qt_button_box_on_accepted(qt_button_box_t bb,
                                           qt_callback_void callback,
                                           long callback_id) {
    QObject::connect(static_cast<QDialogButtonBox*>(bb),
                     &QDialogButtonBox::accepted,
                     [callback, callback_id]() {
                         callback(callback_id);
                     });
}

extern "C" void qt_button_box_on_rejected(qt_button_box_t bb,
                                           qt_callback_void callback,
                                           long callback_id) {
    QObject::connect(static_cast<QDialogButtonBox*>(bb),
                     &QDialogButtonBox::rejected,
                     [callback, callback_id]() {
                         callback(callback_id);
                     });
}

extern "C" void qt_button_box_on_clicked(qt_button_box_t bb,
                                          qt_callback_void callback,
                                          long callback_id) {
    QObject::connect(static_cast<QDialogButtonBox*>(bb),
                     &QDialogButtonBox::clicked,
                     [callback, callback_id](QAbstractButton*) {
                         callback(callback_id);
                     });
}

// ============================================================
// Phase 10: Calendar Widget
// ============================================================

extern "C" qt_calendar_t qt_calendar_create(qt_widget_t parent) {
    return new QCalendarWidget(static_cast<QWidget*>(parent));
}

extern "C" void qt_calendar_set_selected_date(qt_calendar_t c,
                                               int year, int month, int day) {
    static_cast<QCalendarWidget*>(c)->setSelectedDate(
        QDate(year, month, day));
}

extern "C" int qt_calendar_selected_year(qt_calendar_t c) {
    return static_cast<QCalendarWidget*>(c)->selectedDate().year();
}

extern "C" int qt_calendar_selected_month(qt_calendar_t c) {
    return static_cast<QCalendarWidget*>(c)->selectedDate().month();
}

extern "C" int qt_calendar_selected_day(qt_calendar_t c) {
    return static_cast<QCalendarWidget*>(c)->selectedDate().day();
}

extern "C" const char* qt_calendar_selected_date_string(qt_calendar_t c) {
    s_return_buf = static_cast<QCalendarWidget*>(c)->selectedDate()
                       .toString(Qt::ISODate).toUtf8().toStdString();
    return s_return_buf.c_str();
}

extern "C" void qt_calendar_set_minimum_date(qt_calendar_t c,
                                              int year, int month, int day) {
    static_cast<QCalendarWidget*>(c)->setMinimumDate(
        QDate(year, month, day));
}

extern "C" void qt_calendar_set_maximum_date(qt_calendar_t c,
                                              int year, int month, int day) {
    static_cast<QCalendarWidget*>(c)->setMaximumDate(
        QDate(year, month, day));
}

extern "C" void qt_calendar_set_first_day_of_week(qt_calendar_t c, int day) {
    static_cast<QCalendarWidget*>(c)->setFirstDayOfWeek(
        static_cast<Qt::DayOfWeek>(day));
}

extern "C" void qt_calendar_set_grid_visible(qt_calendar_t c, int visible) {
    static_cast<QCalendarWidget*>(c)->setGridVisible(visible != 0);
}

extern "C" int qt_calendar_is_grid_visible(qt_calendar_t c) {
    return static_cast<QCalendarWidget*>(c)->isGridVisible() ? 1 : 0;
}

extern "C" void qt_calendar_set_navigation_bar_visible(qt_calendar_t c,
                                                         int visible) {
    static_cast<QCalendarWidget*>(c)->setNavigationBarVisible(visible != 0);
}

extern "C" void qt_calendar_on_selection_changed(qt_calendar_t c,
                                                   qt_callback_void callback,
                                                   long callback_id) {
    QObject::connect(static_cast<QCalendarWidget*>(c),
                     &QCalendarWidget::selectionChanged,
                     [callback, callback_id]() {
                         callback(callback_id);
                     });
}

extern "C" void qt_calendar_on_clicked(qt_calendar_t c,
                                        qt_callback_string callback,
                                        long callback_id) {
    QObject::connect(static_cast<QCalendarWidget*>(c),
                     &QCalendarWidget::clicked,
                     [callback, callback_id](const QDate& date) {
                         std::string iso = date.toString(Qt::ISODate)
                                               .toUtf8().toStdString();
                         callback(callback_id, iso.c_str());
                     });
}
