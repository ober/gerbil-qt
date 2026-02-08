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
#include <QSettings>
#include <QCompleter>
#include <QStringListModel>
#include <QToolTip>
#include <QWhatsThis>
#include <QStandardItemModel>
#include <QStandardItem>
#include <QSortFilterProxyModel>
#include <QListView>
#include <QTableView>
#include <QTreeView>
#include <QItemSelectionModel>
#include <QRegularExpression>
#include <QPlainTextEdit>
#include <QToolButton>
#include <QIntValidator>
#include <QDoubleValidator>
#include <QRegularExpressionValidator>
#include <QSizePolicy>
#include <QBoxLayout>
#include <QProcess>
#include <QWizard>
#include <QWizardPage>
#include <QMdiArea>
#include <QMdiSubWindow>
#include <QDial>
#include <QLCDNumber>
#include <QToolBox>
#include <QUndoStack>
#include <QUndoCommand>
#include <QFileSystemModel>
#include <QGraphicsScene>
#include <QGraphicsView>
#include <QGraphicsRectItem>
#include <QGraphicsEllipseItem>
#include <QGraphicsLineItem>
#include <QGraphicsTextItem>
#include <QGraphicsPixmapItem>
#include <string>
#include <cstdio>
#include <sys/wait.h>
#include <signal.h>

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

// Thread-local storage for view signal row/col (Phase 12)
static thread_local int s_last_view_row = -1;
static thread_local int s_last_view_col = -1;

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

extern "C" int qt_widget_is_visible(qt_widget_t w) {
    return static_cast<QWidget*>(w)->isVisible() ? 1 : 0;
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
                         std::string s = text.toUtf8().toStdString();
                         callback(callback_id, s.c_str());
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

// ============================================================
// Phase 11: QSettings
// ============================================================

extern "C" qt_settings_t qt_settings_create(const char* org, const char* app) {
    return new QSettings(QString::fromUtf8(org), QString::fromUtf8(app));
}

extern "C" qt_settings_t qt_settings_create_file(const char* path, int format) {
    return new QSettings(QString::fromUtf8(path),
                         static_cast<QSettings::Format>(format));
}

extern "C" void qt_settings_set_string(qt_settings_t s, const char* key,
                                        const char* value) {
    static_cast<QSettings*>(s)->setValue(
        QString::fromUtf8(key), QString::fromUtf8(value));
}

extern "C" const char* qt_settings_value_string(qt_settings_t s,
                                                  const char* key,
                                                  const char* default_value) {
    s_return_buf = static_cast<QSettings*>(s)->value(
        QString::fromUtf8(key),
        QString::fromUtf8(default_value)).toString().toUtf8().toStdString();
    return s_return_buf.c_str();
}

extern "C" void qt_settings_set_int(qt_settings_t s, const char* key,
                                     int value) {
    static_cast<QSettings*>(s)->setValue(QString::fromUtf8(key), value);
}

extern "C" int qt_settings_value_int(qt_settings_t s, const char* key,
                                      int default_value) {
    return static_cast<QSettings*>(s)->value(
        QString::fromUtf8(key), default_value).toInt();
}

extern "C" void qt_settings_set_double(qt_settings_t s, const char* key,
                                        double value) {
    static_cast<QSettings*>(s)->setValue(QString::fromUtf8(key), value);
}

extern "C" double qt_settings_value_double(qt_settings_t s, const char* key,
                                            double default_value) {
    return static_cast<QSettings*>(s)->value(
        QString::fromUtf8(key), default_value).toDouble();
}

extern "C" void qt_settings_set_bool(qt_settings_t s, const char* key,
                                      int value) {
    static_cast<QSettings*>(s)->setValue(
        QString::fromUtf8(key), value != 0);
}

extern "C" int qt_settings_value_bool(qt_settings_t s, const char* key,
                                       int default_value) {
    return static_cast<QSettings*>(s)->value(
        QString::fromUtf8(key), default_value != 0).toBool() ? 1 : 0;
}

extern "C" int qt_settings_contains(qt_settings_t s, const char* key) {
    return static_cast<QSettings*>(s)->contains(
        QString::fromUtf8(key)) ? 1 : 0;
}

extern "C" void qt_settings_remove(qt_settings_t s, const char* key) {
    static_cast<QSettings*>(s)->remove(QString::fromUtf8(key));
}

extern "C" const char* qt_settings_all_keys(qt_settings_t s) {
    QStringList keys = static_cast<QSettings*>(s)->allKeys();
    s_return_buf = keys.join('\n').toUtf8().toStdString();
    return s_return_buf.c_str();
}

extern "C" const char* qt_settings_child_keys(qt_settings_t s) {
    QStringList keys = static_cast<QSettings*>(s)->childKeys();
    s_return_buf = keys.join('\n').toUtf8().toStdString();
    return s_return_buf.c_str();
}

extern "C" const char* qt_settings_child_groups(qt_settings_t s) {
    QStringList groups = static_cast<QSettings*>(s)->childGroups();
    s_return_buf = groups.join('\n').toUtf8().toStdString();
    return s_return_buf.c_str();
}

extern "C" void qt_settings_begin_group(qt_settings_t s, const char* prefix) {
    static_cast<QSettings*>(s)->beginGroup(QString::fromUtf8(prefix));
}

extern "C" void qt_settings_end_group(qt_settings_t s) {
    static_cast<QSettings*>(s)->endGroup();
}

extern "C" const char* qt_settings_group(qt_settings_t s) {
    s_return_buf = static_cast<QSettings*>(s)->group().toUtf8().toStdString();
    return s_return_buf.c_str();
}

extern "C" void qt_settings_sync(qt_settings_t s) {
    static_cast<QSettings*>(s)->sync();
}

extern "C" void qt_settings_clear(qt_settings_t s) {
    static_cast<QSettings*>(s)->clear();
}

extern "C" const char* qt_settings_file_name(qt_settings_t s) {
    s_return_buf = static_cast<QSettings*>(s)->fileName()
                       .toUtf8().toStdString();
    return s_return_buf.c_str();
}

extern "C" int qt_settings_is_writable(qt_settings_t s) {
    return static_cast<QSettings*>(s)->isWritable() ? 1 : 0;
}

extern "C" void qt_settings_destroy(qt_settings_t s) {
    delete static_cast<QSettings*>(s);
}

// ============================================================
// Phase 11: QCompleter
// ============================================================

extern "C" qt_completer_t qt_completer_create(const char* items_newline) {
    QStringList items = QString::fromUtf8(items_newline)
                            .split('\n', Qt::SkipEmptyParts);
    auto* completer = new QCompleter(items);
    return completer;
}

extern "C" void qt_completer_set_model_strings(qt_completer_t c,
                                                const char* items_newline) {
    QStringList items = QString::fromUtf8(items_newline)
                            .split('\n', Qt::SkipEmptyParts);
    auto* completer = static_cast<QCompleter*>(c);
    auto* model = qobject_cast<QStringListModel*>(completer->model());
    if (model) {
        model->setStringList(items);
    } else {
        completer->setModel(new QStringListModel(items, completer));
    }
}

extern "C" void qt_completer_set_case_sensitivity(qt_completer_t c, int cs) {
    static_cast<QCompleter*>(c)->setCaseSensitivity(
        cs != 0 ? Qt::CaseSensitive : Qt::CaseInsensitive);
}

extern "C" void qt_completer_set_completion_mode(qt_completer_t c, int mode) {
    static_cast<QCompleter*>(c)->setCompletionMode(
        static_cast<QCompleter::CompletionMode>(mode));
}

extern "C" void qt_completer_set_filter_mode(qt_completer_t c, int mode) {
    Qt::MatchFlags flags;
    switch (mode) {
        case 0: flags = Qt::MatchStartsWith; break;
        case 1: flags = Qt::MatchContains; break;
        case 2: flags = Qt::MatchEndsWith; break;
        default: flags = Qt::MatchStartsWith; break;
    }
    static_cast<QCompleter*>(c)->setFilterMode(flags);
}

extern "C" void qt_completer_set_max_visible_items(qt_completer_t c,
                                                     int count) {
    static_cast<QCompleter*>(c)->setMaxVisibleItems(count);
}

extern "C" int qt_completer_completion_count(qt_completer_t c) {
    return static_cast<QCompleter*>(c)->completionCount();
}

extern "C" const char* qt_completer_current_completion(qt_completer_t c) {
    s_return_buf = static_cast<QCompleter*>(c)->currentCompletion()
                       .toUtf8().toStdString();
    return s_return_buf.c_str();
}

extern "C" void qt_completer_set_completion_prefix(qt_completer_t c,
                                                    const char* prefix) {
    static_cast<QCompleter*>(c)->setCompletionPrefix(
        QString::fromUtf8(prefix));
}

extern "C" void qt_completer_on_activated(qt_completer_t c,
                                           qt_callback_string callback,
                                           long callback_id) {
    QObject::connect(static_cast<QCompleter*>(c),
                     QOverload<const QString&>::of(&QCompleter::activated),
                     [callback, callback_id](const QString& text) {
                         std::string s = text.toUtf8().toStdString();
                         callback(callback_id, s.c_str());
                     });
}

extern "C" void qt_line_edit_set_completer(qt_line_edit_t e,
                                            qt_completer_t c) {
    static_cast<QLineEdit*>(e)->setCompleter(
        static_cast<QCompleter*>(c));
}

extern "C" void qt_completer_destroy(qt_completer_t c) {
    delete static_cast<QCompleter*>(c);
}

// ============================================================
// Phase 11: QToolTip / QWhatsThis
// ============================================================

extern "C" void qt_tooltip_show_text(int x, int y, const char* text,
                                      qt_widget_t widget) {
    QToolTip::showText(QPoint(x, y), QString::fromUtf8(text),
                       static_cast<QWidget*>(widget));
}

extern "C" void qt_tooltip_hide_text(void) {
    QToolTip::hideText();
}

extern "C" int qt_tooltip_is_visible(void) {
    return QToolTip::isVisible() ? 1 : 0;
}

extern "C" const char* qt_widget_tooltip(qt_widget_t w) {
    s_return_buf = static_cast<QWidget*>(w)->toolTip().toUtf8().toStdString();
    return s_return_buf.c_str();
}

extern "C" void qt_widget_set_whats_this(qt_widget_t w, const char* text) {
    static_cast<QWidget*>(w)->setWhatsThis(QString::fromUtf8(text));
}

extern "C" const char* qt_widget_whats_this(qt_widget_t w) {
    s_return_buf = static_cast<QWidget*>(w)->whatsThis()
                       .toUtf8().toStdString();
    return s_return_buf.c_str();
}

/* ================================================================
   Phase 12: Model/View Framework
   ================================================================ */

/* --- QStandardItemModel --- */

extern "C" qt_standard_model_t qt_standard_model_create(int rows, int cols,
                                                          qt_widget_t parent) {
    auto* p = parent ? static_cast<QWidget*>(parent) : nullptr;
    return new QStandardItemModel(rows, cols, p);
}

extern "C" void qt_standard_model_destroy(qt_standard_model_t m) {
    delete static_cast<QStandardItemModel*>(m);
}

extern "C" int qt_standard_model_row_count(qt_standard_model_t m) {
    return static_cast<QStandardItemModel*>(m)->rowCount();
}

extern "C" int qt_standard_model_column_count(qt_standard_model_t m) {
    return static_cast<QStandardItemModel*>(m)->columnCount();
}

extern "C" void qt_standard_model_set_row_count(qt_standard_model_t m, int rows) {
    static_cast<QStandardItemModel*>(m)->setRowCount(rows);
}

extern "C" void qt_standard_model_set_column_count(qt_standard_model_t m, int cols) {
    static_cast<QStandardItemModel*>(m)->setColumnCount(cols);
}

extern "C" void qt_standard_model_set_item(qt_standard_model_t m, int row,
                                             int col, qt_standard_item_t item) {
    static_cast<QStandardItemModel*>(m)->setItem(
        row, col, static_cast<QStandardItem*>(item));
}

extern "C" qt_standard_item_t qt_standard_model_item(qt_standard_model_t m,
                                                       int row, int col) {
    return static_cast<QStandardItemModel*>(m)->item(row, col);
}

extern "C" int qt_standard_model_insert_row(qt_standard_model_t m, int row) {
    return static_cast<QStandardItemModel*>(m)->insertRow(row) ? 1 : 0;
}

extern "C" int qt_standard_model_insert_column(qt_standard_model_t m, int col) {
    return static_cast<QStandardItemModel*>(m)->insertColumn(col) ? 1 : 0;
}

extern "C" int qt_standard_model_remove_row(qt_standard_model_t m, int row) {
    return static_cast<QStandardItemModel*>(m)->removeRow(row) ? 1 : 0;
}

extern "C" int qt_standard_model_remove_column(qt_standard_model_t m, int col) {
    return static_cast<QStandardItemModel*>(m)->removeColumn(col) ? 1 : 0;
}

extern "C" void qt_standard_model_clear(qt_standard_model_t m) {
    static_cast<QStandardItemModel*>(m)->clear();
}

extern "C" void qt_standard_model_set_horizontal_header(qt_standard_model_t m,
                                                          int col,
                                                          const char* text) {
    static_cast<QStandardItemModel*>(m)->setHorizontalHeaderItem(
        col, new QStandardItem(QString::fromUtf8(text)));
}

extern "C" void qt_standard_model_set_vertical_header(qt_standard_model_t m,
                                                        int row,
                                                        const char* text) {
    static_cast<QStandardItemModel*>(m)->setVerticalHeaderItem(
        row, new QStandardItem(QString::fromUtf8(text)));
}

/* --- QStandardItem --- */

extern "C" qt_standard_item_t qt_standard_item_create(const char* text) {
    return new QStandardItem(QString::fromUtf8(text));
}

extern "C" const char* qt_standard_item_text(qt_standard_item_t item) {
    s_return_buf = static_cast<QStandardItem*>(item)->text()
                       .toUtf8().toStdString();
    return s_return_buf.c_str();
}

extern "C" void qt_standard_item_set_text(qt_standard_item_t item,
                                            const char* text) {
    static_cast<QStandardItem*>(item)->setText(QString::fromUtf8(text));
}

extern "C" const char* qt_standard_item_tooltip(qt_standard_item_t item) {
    s_return_buf = static_cast<QStandardItem*>(item)->toolTip()
                       .toUtf8().toStdString();
    return s_return_buf.c_str();
}

extern "C" void qt_standard_item_set_tooltip(qt_standard_item_t item,
                                               const char* text) {
    static_cast<QStandardItem*>(item)->setToolTip(QString::fromUtf8(text));
}

extern "C" void qt_standard_item_set_editable(qt_standard_item_t item, int val) {
    static_cast<QStandardItem*>(item)->setEditable(val != 0);
}

extern "C" int qt_standard_item_is_editable(qt_standard_item_t item) {
    return static_cast<QStandardItem*>(item)->isEditable() ? 1 : 0;
}

extern "C" void qt_standard_item_set_enabled(qt_standard_item_t item, int val) {
    static_cast<QStandardItem*>(item)->setEnabled(val != 0);
}

extern "C" int qt_standard_item_is_enabled(qt_standard_item_t item) {
    return static_cast<QStandardItem*>(item)->isEnabled() ? 1 : 0;
}

extern "C" void qt_standard_item_set_selectable(qt_standard_item_t item, int val) {
    static_cast<QStandardItem*>(item)->setSelectable(val != 0);
}

extern "C" int qt_standard_item_is_selectable(qt_standard_item_t item) {
    return static_cast<QStandardItem*>(item)->isSelectable() ? 1 : 0;
}

extern "C" void qt_standard_item_set_checkable(qt_standard_item_t item, int val) {
    static_cast<QStandardItem*>(item)->setCheckable(val != 0);
}

extern "C" int qt_standard_item_is_checkable(qt_standard_item_t item) {
    return static_cast<QStandardItem*>(item)->isCheckable() ? 1 : 0;
}

extern "C" void qt_standard_item_set_check_state(qt_standard_item_t item,
                                                    int state) {
    static_cast<QStandardItem*>(item)->setCheckState(
        static_cast<Qt::CheckState>(state));
}

extern "C" int qt_standard_item_check_state(qt_standard_item_t item) {
    return static_cast<int>(
        static_cast<QStandardItem*>(item)->checkState());
}

extern "C" void qt_standard_item_set_icon(qt_standard_item_t item, void* icon) {
    static_cast<QStandardItem*>(item)->setIcon(
        *static_cast<QIcon*>(icon));
}

extern "C" void qt_standard_item_append_row(qt_standard_item_t parent,
                                              qt_standard_item_t child) {
    static_cast<QStandardItem*>(parent)->appendRow(
        static_cast<QStandardItem*>(child));
}

extern "C" int qt_standard_item_row_count(qt_standard_item_t item) {
    return static_cast<QStandardItem*>(item)->rowCount();
}

extern "C" int qt_standard_item_column_count(qt_standard_item_t item) {
    return static_cast<QStandardItem*>(item)->columnCount();
}

extern "C" qt_standard_item_t qt_standard_item_child(qt_standard_item_t item,
                                                       int row, int col) {
    return static_cast<QStandardItem*>(item)->child(row, col);
}

/* --- QStringListModel --- */

extern "C" qt_string_list_model_t qt_string_list_model_create(
        const char* items_newline) {
    auto* m = new QStringListModel();
    if (items_newline && items_newline[0]) {
        m->setStringList(QString::fromUtf8(items_newline)
                             .split('\n', Qt::SkipEmptyParts));
    }
    return m;
}

extern "C" void qt_string_list_model_destroy(qt_string_list_model_t m) {
    delete static_cast<QStringListModel*>(m);
}

extern "C" void qt_string_list_model_set_strings(qt_string_list_model_t m,
                                                   const char* items_newline) {
    QStringList list;
    if (items_newline && items_newline[0]) {
        list = QString::fromUtf8(items_newline)
                   .split('\n', Qt::SkipEmptyParts);
    }
    static_cast<QStringListModel*>(m)->setStringList(list);
}

extern "C" const char* qt_string_list_model_strings(qt_string_list_model_t m) {
    s_return_buf = static_cast<QStringListModel*>(m)->stringList()
                       .join('\n').toUtf8().toStdString();
    return s_return_buf.c_str();
}

extern "C" int qt_string_list_model_row_count(qt_string_list_model_t m) {
    return static_cast<QStringListModel*>(m)->rowCount();
}

/* --- Common view functions (QAbstractItemView) --- */

extern "C" void qt_view_set_model(qt_widget_t view, void* model) {
    static_cast<QAbstractItemView*>(static_cast<QWidget*>(view))
        ->setModel(static_cast<QAbstractItemModel*>(model));
}

extern "C" void qt_view_set_selection_mode(qt_widget_t view, int mode) {
    static_cast<QAbstractItemView*>(static_cast<QWidget*>(view))
        ->setSelectionMode(
            static_cast<QAbstractItemView::SelectionMode>(mode));
}

extern "C" void qt_view_set_selection_behavior(qt_widget_t view, int behavior) {
    static_cast<QAbstractItemView*>(static_cast<QWidget*>(view))
        ->setSelectionBehavior(
            static_cast<QAbstractItemView::SelectionBehavior>(behavior));
}

extern "C" void qt_view_set_alternating_row_colors(qt_widget_t view, int val) {
    static_cast<QAbstractItemView*>(static_cast<QWidget*>(view))
        ->setAlternatingRowColors(val != 0);
}

extern "C" void qt_view_set_sorting_enabled(qt_widget_t view, int val) {
    // QAbstractItemView doesn't have setSortingEnabled directly;
    // QTableView and QTreeView do. Use dynamic_cast to detect.
    auto* tv = dynamic_cast<QTableView*>(static_cast<QWidget*>(view));
    if (tv) { tv->setSortingEnabled(val != 0); return; }
    auto* trv = dynamic_cast<QTreeView*>(static_cast<QWidget*>(view));
    if (trv) { trv->setSortingEnabled(val != 0); }
}

extern "C" void qt_view_set_edit_triggers(qt_widget_t view, int triggers) {
    static_cast<QAbstractItemView*>(static_cast<QWidget*>(view))
        ->setEditTriggers(
            static_cast<QAbstractItemView::EditTriggers>(triggers));
}

/* --- QListView --- */

extern "C" qt_list_view_t qt_list_view_create(qt_widget_t parent) {
    auto* p = parent ? static_cast<QWidget*>(parent) : nullptr;
    return new QListView(p);
}

extern "C" void qt_list_view_set_flow(qt_list_view_t v, int flow) {
    static_cast<QListView*>(v)->setFlow(
        static_cast<QListView::Flow>(flow));
}

/* --- QTableView --- */

extern "C" qt_table_view_t qt_table_view_create(qt_widget_t parent) {
    auto* p = parent ? static_cast<QWidget*>(parent) : nullptr;
    return new QTableView(p);
}

extern "C" void qt_table_view_set_column_width(qt_table_view_t v,
                                                 int col, int w) {
    static_cast<QTableView*>(v)->setColumnWidth(col, w);
}

extern "C" void qt_table_view_set_row_height(qt_table_view_t v,
                                               int row, int h) {
    static_cast<QTableView*>(v)->setRowHeight(row, h);
}

extern "C" void qt_table_view_hide_column(qt_table_view_t v, int col) {
    static_cast<QTableView*>(v)->hideColumn(col);
}

extern "C" void qt_table_view_show_column(qt_table_view_t v, int col) {
    static_cast<QTableView*>(v)->showColumn(col);
}

extern "C" void qt_table_view_hide_row(qt_table_view_t v, int row) {
    static_cast<QTableView*>(v)->hideRow(row);
}

extern "C" void qt_table_view_show_row(qt_table_view_t v, int row) {
    static_cast<QTableView*>(v)->showRow(row);
}

extern "C" void qt_table_view_resize_columns_to_contents(qt_table_view_t v) {
    static_cast<QTableView*>(v)->resizeColumnsToContents();
}

extern "C" void qt_table_view_resize_rows_to_contents(qt_table_view_t v) {
    static_cast<QTableView*>(v)->resizeRowsToContents();
}

/* --- QTreeView --- */

extern "C" qt_tree_view_t qt_tree_view_create(qt_widget_t parent) {
    auto* p = parent ? static_cast<QWidget*>(parent) : nullptr;
    return new QTreeView(p);
}

extern "C" void qt_tree_view_expand_all(qt_tree_view_t v) {
    static_cast<QTreeView*>(v)->expandAll();
}

extern "C" void qt_tree_view_collapse_all(qt_tree_view_t v) {
    static_cast<QTreeView*>(v)->collapseAll();
}

extern "C" void qt_tree_view_set_indentation(qt_tree_view_t v, int indent) {
    static_cast<QTreeView*>(v)->setIndentation(indent);
}

extern "C" int qt_tree_view_indentation(qt_tree_view_t v) {
    return static_cast<QTreeView*>(v)->indentation();
}

extern "C" void qt_tree_view_set_root_is_decorated(qt_tree_view_t v, int val) {
    static_cast<QTreeView*>(v)->setRootIsDecorated(val != 0);
}

extern "C" void qt_tree_view_set_header_hidden(qt_tree_view_t v, int val) {
    static_cast<QTreeView*>(v)->setHeaderHidden(val != 0);
}

extern "C" void qt_tree_view_set_column_width(qt_tree_view_t v, int col, int w) {
    static_cast<QTreeView*>(v)->setColumnWidth(col, w);
}

/* --- QHeaderView (via view) --- */

static QHeaderView* get_header(qt_widget_t view, int horizontal) {
    if (horizontal) {
        auto* tv = dynamic_cast<QTableView*>(static_cast<QWidget*>(view));
        if (tv) return tv->horizontalHeader();
        auto* trv = dynamic_cast<QTreeView*>(static_cast<QWidget*>(view));
        if (trv) return trv->header();
    } else {
        auto* tv = dynamic_cast<QTableView*>(static_cast<QWidget*>(view));
        if (tv) return tv->verticalHeader();
    }
    return nullptr;
}

extern "C" void qt_view_header_set_stretch_last_section(qt_widget_t view,
                                                          int horizontal,
                                                          int val) {
    auto* h = get_header(view, horizontal);
    if (h) h->setStretchLastSection(val != 0);
}

extern "C" void qt_view_header_set_section_resize_mode(qt_widget_t view,
                                                         int horizontal,
                                                         int mode) {
    auto* h = get_header(view, horizontal);
    if (h) h->setSectionResizeMode(
        static_cast<QHeaderView::ResizeMode>(mode));
}

extern "C" void qt_view_header_hide(qt_widget_t view, int horizontal) {
    auto* h = get_header(view, horizontal);
    if (h) h->hide();
}

extern "C" void qt_view_header_show(qt_widget_t view, int horizontal) {
    auto* h = get_header(view, horizontal);
    if (h) h->show();
}

extern "C" void qt_view_header_set_default_section_size(qt_widget_t view,
                                                          int horizontal,
                                                          int size) {
    auto* h = get_header(view, horizontal);
    if (h) h->setDefaultSectionSize(size);
}

/* --- QSortFilterProxyModel --- */

extern "C" qt_sort_filter_proxy_t qt_sort_filter_proxy_create(void* parent) {
    auto* p = parent ? static_cast<QObject*>(parent) : nullptr;
    return new QSortFilterProxyModel(p);
}

extern "C" void qt_sort_filter_proxy_destroy(qt_sort_filter_proxy_t p) {
    delete static_cast<QSortFilterProxyModel*>(p);
}

extern "C" void qt_sort_filter_proxy_set_source_model(
        qt_sort_filter_proxy_t p, void* model) {
    static_cast<QSortFilterProxyModel*>(p)->setSourceModel(
        static_cast<QAbstractItemModel*>(model));
}

extern "C" void qt_sort_filter_proxy_set_filter_regex(
        qt_sort_filter_proxy_t p, const char* pattern) {
    static_cast<QSortFilterProxyModel*>(p)->setFilterRegularExpression(
        QString::fromUtf8(pattern));
}

extern "C" void qt_sort_filter_proxy_set_filter_column(
        qt_sort_filter_proxy_t p, int col) {
    static_cast<QSortFilterProxyModel*>(p)->setFilterKeyColumn(col);
}

extern "C" void qt_sort_filter_proxy_set_filter_case_sensitivity(
        qt_sort_filter_proxy_t p, int cs) {
    static_cast<QSortFilterProxyModel*>(p)->setFilterCaseSensitivity(
        static_cast<Qt::CaseSensitivity>(cs));
}

extern "C" void qt_sort_filter_proxy_set_filter_role(
        qt_sort_filter_proxy_t p, int role) {
    static_cast<QSortFilterProxyModel*>(p)->setFilterRole(role);
}

extern "C" void qt_sort_filter_proxy_sort(qt_sort_filter_proxy_t p,
                                            int col, int order) {
    static_cast<QSortFilterProxyModel*>(p)->sort(
        col, static_cast<Qt::SortOrder>(order));
}

extern "C" void qt_sort_filter_proxy_set_sort_role(
        qt_sort_filter_proxy_t p, int role) {
    static_cast<QSortFilterProxyModel*>(p)->setSortRole(role);
}

extern "C" void qt_sort_filter_proxy_set_dynamic_sort_filter(
        qt_sort_filter_proxy_t p, int val) {
    static_cast<QSortFilterProxyModel*>(p)->setDynamicSortFilter(val != 0);
}

extern "C" void qt_sort_filter_proxy_invalidate_filter(
        qt_sort_filter_proxy_t p) {
    static_cast<QSortFilterProxyModel*>(p)->invalidate();
}

extern "C" int qt_sort_filter_proxy_row_count(qt_sort_filter_proxy_t p) {
    return static_cast<QSortFilterProxyModel*>(p)->rowCount();
}

/* --- View signals + selection --- */

extern "C" void qt_view_on_clicked(qt_widget_t view,
                                     qt_callback_void callback,
                                     long callback_id) {
    auto* v = static_cast<QAbstractItemView*>(static_cast<QWidget*>(view));
    QObject::connect(v, &QAbstractItemView::clicked,
        [callback, callback_id](const QModelIndex& idx) {
            s_last_view_row = idx.row();
            s_last_view_col = idx.column();
            callback(callback_id);
        });
}

extern "C" void qt_view_on_double_clicked(qt_widget_t view,
                                            qt_callback_void callback,
                                            long callback_id) {
    auto* v = static_cast<QAbstractItemView*>(static_cast<QWidget*>(view));
    QObject::connect(v, &QAbstractItemView::doubleClicked,
        [callback, callback_id](const QModelIndex& idx) {
            s_last_view_row = idx.row();
            s_last_view_col = idx.column();
            callback(callback_id);
        });
}

extern "C" void qt_view_on_activated(qt_widget_t view,
                                       qt_callback_void callback,
                                       long callback_id) {
    auto* v = static_cast<QAbstractItemView*>(static_cast<QWidget*>(view));
    QObject::connect(v, &QAbstractItemView::activated,
        [callback, callback_id](const QModelIndex& idx) {
            s_last_view_row = idx.row();
            s_last_view_col = idx.column();
            callback(callback_id);
        });
}

extern "C" void qt_view_on_selection_changed(qt_widget_t view,
                                               qt_callback_void callback,
                                               long callback_id) {
    auto* v = static_cast<QAbstractItemView*>(static_cast<QWidget*>(view));
    auto* sel = v->selectionModel();
    if (!sel) return;
    QObject::connect(sel, &QItemSelectionModel::selectionChanged,
        [callback, callback_id](const QItemSelection&, const QItemSelection&) {
            callback(callback_id);
        });
}

extern "C" int qt_view_last_clicked_row(void) {
    return s_last_view_row;
}

extern "C" int qt_view_last_clicked_col(void) {
    return s_last_view_col;
}

extern "C" const char* qt_view_selected_rows(qt_widget_t view) {
    auto* v = static_cast<QAbstractItemView*>(static_cast<QWidget*>(view));
    auto* sel = v->selectionModel();
    if (!sel) { s_return_buf.clear(); return s_return_buf.c_str(); }
    auto rows = sel->selectedRows();
    std::string result;
    for (int i = 0; i < rows.size(); i++) {
        if (i > 0) result += '\n';
        result += std::to_string(rows[i].row());
    }
    s_return_buf = result;
    return s_return_buf.c_str();
}

extern "C" int qt_view_current_row(qt_widget_t view) {
    auto* v = static_cast<QAbstractItemView*>(static_cast<QWidget*>(view));
    auto idx = v->currentIndex();
    return idx.isValid() ? idx.row() : -1;
}

/* ========== Phase 13: Practical Polish ========== */

/* --- QValidator --- */

extern "C" qt_validator_t qt_int_validator_create(int minimum, int maximum,
                                                    qt_widget_t parent) {
    auto* p = parent ? static_cast<QWidget*>(parent) : nullptr;
    return static_cast<void*>(new QIntValidator(minimum, maximum,
                              static_cast<QObject*>(p)));
}

extern "C" qt_validator_t qt_double_validator_create(double bottom, double top,
                                                       int decimals,
                                                       qt_widget_t parent) {
    auto* p = parent ? static_cast<QWidget*>(parent) : nullptr;
    return static_cast<void*>(new QDoubleValidator(bottom, top, decimals,
                              static_cast<QObject*>(p)));
}

extern "C" qt_validator_t qt_regex_validator_create(const char* pattern,
                                                      qt_widget_t parent) {
    auto* p = parent ? static_cast<QWidget*>(parent) : nullptr;
    QRegularExpression re(QString::fromUtf8(pattern));
    return static_cast<void*>(new QRegularExpressionValidator(re,
                              static_cast<QObject*>(p)));
}

extern "C" void qt_validator_destroy(qt_validator_t v) {
    delete static_cast<QValidator*>(v);
}

extern "C" int qt_validator_validate(qt_validator_t v, const char* input) {
    auto* val = static_cast<QValidator*>(v);
    QString str = QString::fromUtf8(input);
    int pos = 0;
    QValidator::State state = val->validate(str, pos);
    switch (state) {
        case QValidator::Invalid:       return QT_VALIDATOR_INVALID;
        case QValidator::Intermediate:  return QT_VALIDATOR_INTERMEDIATE;
        case QValidator::Acceptable:    return QT_VALIDATOR_ACCEPTABLE;
        default:                        return QT_VALIDATOR_INVALID;
    }
}

extern "C" void qt_line_edit_set_validator(qt_line_edit_t e,
                                            qt_validator_t v) {
    static_cast<QLineEdit*>(e)->setValidator(static_cast<const QValidator*>(v));
}

extern "C" int qt_line_edit_has_acceptable_input(qt_line_edit_t e) {
    return static_cast<QLineEdit*>(e)->hasAcceptableInput() ? 1 : 0;
}

/* --- QPlainTextEdit --- */

extern "C" qt_plain_text_edit_t qt_plain_text_edit_create(qt_widget_t parent) {
    auto* p = parent ? static_cast<QWidget*>(parent) : nullptr;
    return static_cast<void*>(new QPlainTextEdit(p));
}

extern "C" void qt_plain_text_edit_set_text(qt_plain_text_edit_t e,
                                              const char* text) {
    static_cast<QPlainTextEdit*>(e)->setPlainText(QString::fromUtf8(text));
}

extern "C" const char* qt_plain_text_edit_text(qt_plain_text_edit_t e) {
    s_return_buf = static_cast<QPlainTextEdit*>(e)->toPlainText()
                   .toUtf8().constData();
    return s_return_buf.c_str();
}

extern "C" void qt_plain_text_edit_append(qt_plain_text_edit_t e,
                                            const char* text) {
    static_cast<QPlainTextEdit*>(e)->appendPlainText(QString::fromUtf8(text));
}

extern "C" void qt_plain_text_edit_clear(qt_plain_text_edit_t e) {
    static_cast<QPlainTextEdit*>(e)->clear();
}

extern "C" void qt_plain_text_edit_set_read_only(qt_plain_text_edit_t e,
                                                   int read_only) {
    static_cast<QPlainTextEdit*>(e)->setReadOnly(read_only != 0);
}

extern "C" int qt_plain_text_edit_is_read_only(qt_plain_text_edit_t e) {
    return static_cast<QPlainTextEdit*>(e)->isReadOnly() ? 1 : 0;
}

extern "C" void qt_plain_text_edit_set_placeholder(qt_plain_text_edit_t e,
                                                     const char* text) {
    static_cast<QPlainTextEdit*>(e)->setPlaceholderText(
        QString::fromUtf8(text));
}

extern "C" int qt_plain_text_edit_line_count(qt_plain_text_edit_t e) {
    return static_cast<QPlainTextEdit*>(e)->blockCount();
}

extern "C" void qt_plain_text_edit_set_max_block_count(qt_plain_text_edit_t e,
                                                         int count) {
    static_cast<QPlainTextEdit*>(e)->setMaximumBlockCount(count);
}

extern "C" int qt_plain_text_edit_cursor_line(qt_plain_text_edit_t e) {
    auto* pte = static_cast<QPlainTextEdit*>(e);
    QTextCursor tc = pte->textCursor();
    return tc.blockNumber();
}

extern "C" int qt_plain_text_edit_cursor_column(qt_plain_text_edit_t e) {
    auto* pte = static_cast<QPlainTextEdit*>(e);
    QTextCursor tc = pte->textCursor();
    return tc.columnNumber();
}

extern "C" void qt_plain_text_edit_set_line_wrap(qt_plain_text_edit_t e,
                                                   int mode) {
    auto* pte = static_cast<QPlainTextEdit*>(e);
    pte->setLineWrapMode(mode == 0 ? QPlainTextEdit::NoWrap
                                   : QPlainTextEdit::WidgetWidth);
}

extern "C" void qt_plain_text_edit_on_text_changed(qt_plain_text_edit_t e,
                                                     qt_callback_void callback,
                                                     long callback_id) {
    auto* pte = static_cast<QPlainTextEdit*>(e);
    QObject::connect(pte, &QPlainTextEdit::textChanged,
        [callback, callback_id]() { callback(callback_id); });
}

/* --- QToolButton --- */

extern "C" qt_tool_button_t qt_tool_button_create(qt_widget_t parent) {
    auto* p = parent ? static_cast<QWidget*>(parent) : nullptr;
    return static_cast<void*>(new QToolButton(p));
}

extern "C" void qt_tool_button_set_text(qt_tool_button_t b,
                                          const char* text) {
    static_cast<QToolButton*>(b)->setText(QString::fromUtf8(text));
}

extern "C" const char* qt_tool_button_text(qt_tool_button_t b) {
    s_return_buf = static_cast<QToolButton*>(b)->text()
                   .toUtf8().constData();
    return s_return_buf.c_str();
}

extern "C" void qt_tool_button_set_icon(qt_tool_button_t b,
                                          const char* path) {
    static_cast<QToolButton*>(b)->setIcon(
        QIcon(QString::fromUtf8(path)));
}

extern "C" void qt_tool_button_set_menu(qt_tool_button_t b,
                                          qt_widget_t menu) {
    static_cast<QToolButton*>(b)->setMenu(
        static_cast<QMenu*>(static_cast<QWidget*>(menu)));
}

extern "C" void qt_tool_button_set_popup_mode(qt_tool_button_t b, int mode) {
    static_cast<QToolButton*>(b)->setPopupMode(
        static_cast<QToolButton::ToolButtonPopupMode>(mode));
}

extern "C" void qt_tool_button_set_auto_raise(qt_tool_button_t b, int val) {
    static_cast<QToolButton*>(b)->setAutoRaise(val != 0);
}

extern "C" void qt_tool_button_set_arrow_type(qt_tool_button_t b, int arrow) {
    static_cast<QToolButton*>(b)->setArrowType(
        static_cast<Qt::ArrowType>(arrow));
}

extern "C" void qt_tool_button_set_tool_button_style(qt_tool_button_t b,
                                                       int style) {
    static_cast<QToolButton*>(b)->setToolButtonStyle(
        static_cast<Qt::ToolButtonStyle>(style));
}

extern "C" void qt_tool_button_on_clicked(qt_tool_button_t b,
                                            qt_callback_void callback,
                                            long callback_id) {
    auto* btn = static_cast<QToolButton*>(b);
    QObject::connect(btn, &QToolButton::clicked,
        [callback, callback_id]() { callback(callback_id); });
}

/* --- Layout spacers --- */

extern "C" void qt_layout_add_spacing(qt_layout_t layout, int size) {
    auto* box = dynamic_cast<QBoxLayout*>(static_cast<QLayout*>(layout));
    if (box) box->addSpacing(size);
}

/* --- QSizePolicy --- */

extern "C" void qt_widget_set_size_policy(qt_widget_t w, int h_policy,
                                            int v_policy) {
    auto* widget = static_cast<QWidget*>(w);
    widget->setSizePolicy(static_cast<QSizePolicy::Policy>(h_policy),
                          static_cast<QSizePolicy::Policy>(v_policy));
}

extern "C" void qt_layout_set_stretch_factor(qt_layout_t layout,
                                               qt_widget_t widget,
                                               int stretch) {
    auto* box = dynamic_cast<QBoxLayout*>(static_cast<QLayout*>(layout));
    if (box) box->setStretchFactor(static_cast<QWidget*>(widget), stretch);
}

/* ========== Phase 14: Graphics Scene & Custom Painting ========== */

/* --- QGraphicsScene --- */

extern "C" qt_graphics_scene_t qt_graphics_scene_create(double x, double y,
                                                          double w, double h) {
    return static_cast<void*>(new QGraphicsScene(x, y, w, h));
}

extern "C" qt_graphics_item_t qt_graphics_scene_add_rect(
        qt_graphics_scene_t scene, double x, double y, double w, double h) {
    auto* s = static_cast<QGraphicsScene*>(scene);
    return static_cast<void*>(s->addRect(x, y, w, h));
}

extern "C" qt_graphics_item_t qt_graphics_scene_add_ellipse(
        qt_graphics_scene_t scene, double x, double y, double w, double h) {
    auto* s = static_cast<QGraphicsScene*>(scene);
    return static_cast<void*>(s->addEllipse(x, y, w, h));
}

extern "C" qt_graphics_item_t qt_graphics_scene_add_line(
        qt_graphics_scene_t scene, double x1, double y1,
        double x2, double y2) {
    auto* s = static_cast<QGraphicsScene*>(scene);
    return static_cast<void*>(s->addLine(x1, y1, x2, y2));
}

extern "C" qt_graphics_item_t qt_graphics_scene_add_text(
        qt_graphics_scene_t scene, const char* text) {
    auto* s = static_cast<QGraphicsScene*>(scene);
    // QGraphicsTextItem inherits QGraphicsObject (QObject + QGraphicsItem).
    // Must cast to QGraphicsItem* before void* to adjust for multiple inheritance.
    QGraphicsItem* item = s->addText(QString::fromUtf8(text));
    return static_cast<void*>(item);
}

extern "C" qt_graphics_item_t qt_graphics_scene_add_pixmap(
        qt_graphics_scene_t scene, qt_pixmap_t pixmap) {
    auto* s = static_cast<QGraphicsScene*>(scene);
    auto* pm = static_cast<QPixmap*>(pixmap);
    return static_cast<void*>(s->addPixmap(*pm));
}

extern "C" void qt_graphics_scene_remove_item(qt_graphics_scene_t scene,
                                                qt_graphics_item_t item) {
    auto* s = static_cast<QGraphicsScene*>(scene);
    auto* i = static_cast<QGraphicsItem*>(item);
    s->removeItem(i);
    delete i;
}

extern "C" void qt_graphics_scene_clear(qt_graphics_scene_t scene) {
    static_cast<QGraphicsScene*>(scene)->clear();
}

extern "C" int qt_graphics_scene_items_count(qt_graphics_scene_t scene) {
    return static_cast<QGraphicsScene*>(scene)->items().size();
}

extern "C" void qt_graphics_scene_set_background(qt_graphics_scene_t scene,
                                                    int r, int g, int b) {
    static_cast<QGraphicsScene*>(scene)->setBackgroundBrush(QColor(r, g, b));
}

extern "C" void qt_graphics_scene_destroy(qt_graphics_scene_t scene) {
    delete static_cast<QGraphicsScene*>(scene);
}

/* --- QGraphicsView --- */

extern "C" qt_graphics_view_t qt_graphics_view_create(
        qt_graphics_scene_t scene, qt_widget_t parent) {
    auto* s = static_cast<QGraphicsScene*>(scene);
    auto* p = parent ? static_cast<QWidget*>(parent) : nullptr;
    return static_cast<void*>(new QGraphicsView(s, p));
}

extern "C" void qt_graphics_view_set_render_hint(qt_graphics_view_t view,
                                                    int hint, int on) {
    auto* v = static_cast<QGraphicsView*>(static_cast<QWidget*>(view));
    v->setRenderHint(static_cast<QPainter::RenderHint>(hint), on != 0);
}

extern "C" void qt_graphics_view_set_drag_mode(qt_graphics_view_t view,
                                                  int mode) {
    auto* v = static_cast<QGraphicsView*>(static_cast<QWidget*>(view));
    v->setDragMode(static_cast<QGraphicsView::DragMode>(mode));
}

extern "C" void qt_graphics_view_fit_in_view(qt_graphics_view_t view) {
    auto* v = static_cast<QGraphicsView*>(static_cast<QWidget*>(view));
    v->fitInView(v->sceneRect(), Qt::KeepAspectRatio);
}

extern "C" void qt_graphics_view_scale(qt_graphics_view_t view,
                                         double sx, double sy) {
    auto* v = static_cast<QGraphicsView*>(static_cast<QWidget*>(view));
    v->scale(sx, sy);
}

extern "C" void qt_graphics_view_center_on(qt_graphics_view_t view,
                                             double x, double y) {
    auto* v = static_cast<QGraphicsView*>(static_cast<QWidget*>(view));
    v->centerOn(x, y);
}

/* --- QGraphicsItem --- */

extern "C" void qt_graphics_item_set_pos(qt_graphics_item_t item,
                                           double x, double y) {
    static_cast<QGraphicsItem*>(item)->setPos(x, y);
}

extern "C" double qt_graphics_item_x(qt_graphics_item_t item) {
    return static_cast<QGraphicsItem*>(item)->x();
}

extern "C" double qt_graphics_item_y(qt_graphics_item_t item) {
    return static_cast<QGraphicsItem*>(item)->y();
}

extern "C" void qt_graphics_item_set_pen(qt_graphics_item_t item,
                                           int r, int g, int b, int width) {
    auto* gi = static_cast<QGraphicsItem*>(item);
    QPen pen(QColor(r, g, b));
    pen.setWidth(width);
    // Try each abstract shape type
    if (auto* rect = dynamic_cast<QAbstractGraphicsShapeItem*>(gi))
        rect->setPen(pen);
    else if (auto* line = dynamic_cast<QGraphicsLineItem*>(gi))
        line->setPen(pen);
}

extern "C" void qt_graphics_item_set_brush(qt_graphics_item_t item,
                                             int r, int g, int b) {
    auto* gi = static_cast<QGraphicsItem*>(item);
    if (auto* shape = dynamic_cast<QAbstractGraphicsShapeItem*>(gi))
        shape->setBrush(QColor(r, g, b));
}

extern "C" void qt_graphics_item_set_flags(qt_graphics_item_t item,
                                             int flags) {
    auto* gi = static_cast<QGraphicsItem*>(item);
    QGraphicsItem::GraphicsItemFlags f;
    if (flags & QT_ITEM_MOVABLE)    f |= QGraphicsItem::ItemIsMovable;
    if (flags & QT_ITEM_SELECTABLE) f |= QGraphicsItem::ItemIsSelectable;
    if (flags & QT_ITEM_FOCUSABLE)  f |= QGraphicsItem::ItemIsFocusable;
    gi->setFlags(f);
}

extern "C" void qt_graphics_item_set_tooltip(qt_graphics_item_t item,
                                               const char* text) {
    static_cast<QGraphicsItem*>(item)->setToolTip(QString::fromUtf8(text));
}

extern "C" void qt_graphics_item_set_zvalue(qt_graphics_item_t item,
                                              double z) {
    static_cast<QGraphicsItem*>(item)->setZValue(z);
}

extern "C" double qt_graphics_item_zvalue(qt_graphics_item_t item) {
    return static_cast<QGraphicsItem*>(item)->zValue();
}

extern "C" void qt_graphics_item_set_rotation(qt_graphics_item_t item,
                                                double angle) {
    static_cast<QGraphicsItem*>(item)->setRotation(angle);
}

extern "C" void qt_graphics_item_set_scale(qt_graphics_item_t item,
                                             double factor) {
    static_cast<QGraphicsItem*>(item)->setScale(factor);
}

extern "C" void qt_graphics_item_set_visible(qt_graphics_item_t item,
                                               int visible) {
    static_cast<QGraphicsItem*>(item)->setVisible(visible != 0);
}

/* --- PaintWidget (custom paintEvent) --- */

// PaintWidget: QWidget subclass that fires a callback during paintEvent.
// No Q_OBJECT macro needed — just overrides a virtual method.
class PaintWidget : public QWidget {
public:
    PaintWidget(QWidget* parent = nullptr)
        : QWidget(parent), m_callback(nullptr), m_callback_id(0),
          m_painter(nullptr) {}

    void setCallback(qt_callback_void callback, long callback_id) {
        m_callback = callback;
        m_callback_id = callback_id;
    }

    QPainter* currentPainter() const { return m_painter; }

protected:
    void paintEvent(QPaintEvent*) override {
        if (m_callback) {
            QPainter painter(this);
            m_painter = &painter;
            m_callback(m_callback_id);
            m_painter = nullptr;
            // painter.end() is called automatically by QPainter destructor
        }
    }

private:
    qt_callback_void m_callback;
    long m_callback_id;
    QPainter* m_painter;
};

extern "C" qt_paint_widget_t qt_paint_widget_create(qt_widget_t parent) {
    auto* p = parent ? static_cast<QWidget*>(parent) : nullptr;
    return static_cast<void*>(new PaintWidget(p));
}

extern "C" void qt_paint_widget_on_paint(qt_paint_widget_t w,
                                           qt_callback_void callback,
                                           long callback_id) {
    static_cast<PaintWidget*>(static_cast<QWidget*>(w))
        ->setCallback(callback, callback_id);
}

extern "C" qt_painter_t qt_paint_widget_painter(qt_paint_widget_t w) {
    auto* pw = static_cast<PaintWidget*>(static_cast<QWidget*>(w));
    return static_cast<void*>(pw->currentPainter());
}

extern "C" void qt_paint_widget_update(qt_paint_widget_t w) {
    static_cast<QWidget*>(w)->update();
}

extern "C" int qt_paint_widget_width(qt_paint_widget_t w) {
    return static_cast<QWidget*>(w)->width();
}

extern "C" int qt_paint_widget_height(qt_paint_widget_t w) {
    return static_cast<QWidget*>(w)->height();
}

// ============================================================
// Phase 15: QProcess
// ============================================================

// Process exit code tracking — Gambit's SIGCHLD handler reaps child processes
// before QProcess can observe their exit status.  We intercept SIGCHLD with our
// own handler that captures exit codes for tracked PIDs, then forwards to
// Gambit's handler.
// Per-process tracking: supports up to MAX_TRACKED concurrent QProcess instances.
// All state is non-thread-local since Qt must run on a single thread.

// Signal-safe array for the SIGCHLD handler (only sig_atomic_t and simple ops).
static const int MAX_TRACKED_PROCESSES = 16;
struct TrackedProcess {
    volatile sig_atomic_t pid;
    volatile sig_atomic_t exit_code;
};
static TrackedProcess s_tracked_processes[MAX_TRACKED_PROCESSES] = {};

// Non-signal state: per-process callback storage (keyed by QProcess pointer).
struct ProcessInfo {
    qt_callback_int finished_cb;
    long finished_cb_id;
    int last_exit_code;
    pid_t pid;
};
static std::unordered_map<void*, ProcessInfo> s_process_info;

// SIGCHLD handler: only reaps our specifically tracked PIDs (not all children).
// This captures exit codes before Gambit's handler can reap them with waitpid(-1).
// Qt still detects process exit via pipe closure, so waitForFinished works.
static struct sigaction s_gambit_sigaction = {};
static int s_sigchld_refcount = 0;

static void qt_sigchld_handler(int sig) {
    int saved_errno = errno;
    // Only waitpid for our tracked PIDs — never waitpid(-1) which reaps all children
    for (int i = 0; i < MAX_TRACKED_PROCESSES; i++) {
        if (s_tracked_processes[i].pid > 0 && s_tracked_processes[i].exit_code < 0) {
            int wstatus;
            pid_t result = waitpid(s_tracked_processes[i].pid, &wstatus, WNOHANG);
            if (result == s_tracked_processes[i].pid) {
                if (WIFEXITED(wstatus)) {
                    s_tracked_processes[i].exit_code = WEXITSTATUS(wstatus);
                } else if (WIFSIGNALED(wstatus)) {
                    s_tracked_processes[i].exit_code = 128 + WTERMSIG(wstatus);
                }
            }
        }
    }
    // Forward to Gambit's handler so it can handle its own process ports.
    if (s_gambit_sigaction.sa_handler &&
        s_gambit_sigaction.sa_handler != SIG_DFL &&
        s_gambit_sigaction.sa_handler != SIG_IGN) {
        s_gambit_sigaction.sa_handler(sig);
    }
    errno = saved_errno;
}

static void qt_install_sigchld_handler() {
    if (s_sigchld_refcount == 0) {
        struct sigaction sa;
        memset(&sa, 0, sizeof(sa));
        sa.sa_handler = qt_sigchld_handler;
        sa.sa_flags = SA_RESTART | SA_NOCLDSTOP;
        sigemptyset(&sa.sa_mask);
        sigaction(SIGCHLD, &sa, &s_gambit_sigaction);
    }
    s_sigchld_refcount++;
}

static void qt_restore_sigchld_handler() {
    if (s_sigchld_refcount > 0) {
        s_sigchld_refcount--;
        if (s_sigchld_refcount == 0) {
            sigaction(SIGCHLD, &s_gambit_sigaction, nullptr);
        }
    }
}

static int qt_track_pid(pid_t pid) {
    for (int i = 0; i < MAX_TRACKED_PROCESSES; i++) {
        if (s_tracked_processes[i].pid == 0) {
            s_tracked_processes[i].pid = pid;
            s_tracked_processes[i].exit_code = -1;
            return i;
        }
    }
    return -1; // no slot available
}

static int qt_get_tracked_exit_code(pid_t pid) {
    for (int i = 0; i < MAX_TRACKED_PROCESSES; i++) {
        if (s_tracked_processes[i].pid == pid) {
            return s_tracked_processes[i].exit_code;
        }
    }
    return -1;
}

static void qt_untrack_pid(pid_t pid) {
    for (int i = 0; i < MAX_TRACKED_PROCESSES; i++) {
        if (s_tracked_processes[i].pid == pid) {
            s_tracked_processes[i].pid = 0;
            s_tracked_processes[i].exit_code = -1;
            break;
        }
    }
}

extern "C" qt_process_t qt_process_create(qt_widget_t parent) {
    auto* p = parent ? static_cast<QObject*>(static_cast<QWidget*>(parent))
                     : nullptr;
    auto* proc = new QProcess(p);
    return static_cast<void*>(proc);
}

extern "C" void qt_process_start(qt_process_t proc, const char* program,
                                  const char* args_str) {
    QStringList args;
    if (args_str && args_str[0]) {
        args = QString::fromUtf8(args_str).split(
            QChar('\n'), Qt::SkipEmptyParts);
    }

    // Install our SIGCHLD handler to capture exit codes for tracked PIDs
    qt_install_sigchld_handler();

    auto* p = static_cast<QProcess*>(proc);
    p->start(QString::fromUtf8(program), args);
    p->waitForStarted(5000);

    pid_t pid = static_cast<pid_t>(p->processId());
    if (pid > 0) {
        qt_track_pid(pid);
    }
    // Update per-process info (may already have a callback from on_finished)
    auto it = s_process_info.find(proc);
    if (it != s_process_info.end()) {
        it->second.pid = pid;
        it->second.last_exit_code = 0;
    } else {
        s_process_info[proc] = {nullptr, 0, 0, pid};
    }
}

extern "C" void qt_process_write(qt_process_t proc, const char* data) {
    static_cast<QProcess*>(proc)->write(data);
}

extern "C" void qt_process_close_write(qt_process_t proc) {
    static_cast<QProcess*>(proc)->closeWriteChannel();
}

extern "C" const char* qt_process_read_stdout(qt_process_t proc) {
    s_return_buf = static_cast<QProcess*>(proc)
                       ->readAllStandardOutput().toStdString();
    return s_return_buf.c_str();
}

extern "C" const char* qt_process_read_stderr(qt_process_t proc) {
    s_return_buf = static_cast<QProcess*>(proc)
                       ->readAllStandardError().toStdString();
    return s_return_buf.c_str();
}

extern "C" int qt_process_wait_for_finished(qt_process_t proc, int msecs) {
    auto* p = static_cast<QProcess*>(proc);
    pid_t pid = static_cast<pid_t>(p->processId());

    // Let QProcess do its normal waiting (uses pipe closure detection)
    p->waitForFinished(msecs);

    // Determine exit code: prefer our SIGCHLD-captured code (which reaps
    // specifically tracked PIDs before Gambit's handler can), fall back to Qt
    int exit_code = 0;
    int captured = qt_get_tracked_exit_code(pid);
    if (captured >= 0) {
        exit_code = captured;
    } else if (p->state() == QProcess::NotRunning &&
               p->exitStatus() == QProcess::NormalExit) {
        exit_code = p->exitCode();
    }

    bool finished = (p->state() == QProcess::NotRunning) || (captured >= 0);

    // Store exit code and clean up tracking
    if (finished) {
        auto it = s_process_info.find(static_cast<void*>(p));
        if (it != s_process_info.end()) {
            it->second.last_exit_code = exit_code;

            // Manually invoke on-finished callback if registered
            if (it->second.finished_cb) {
                it->second.finished_cb(it->second.finished_cb_id, exit_code);
            }
        }
        qt_untrack_pid(pid);
        qt_restore_sigchld_handler();
    }

    return finished ? 1 : 0;
}

extern "C" int qt_process_exit_code(qt_process_t proc) {
    auto it = s_process_info.find(proc);
    if (it != s_process_info.end()) {
        return it->second.last_exit_code;
    }
    // Fallback: check Qt's own exit code
    return static_cast<QProcess*>(proc)->exitCode();
}

extern "C" int qt_process_state(qt_process_t proc) {
    return static_cast<int>(static_cast<QProcess*>(proc)->state());
}

extern "C" void qt_process_kill(qt_process_t proc) {
    static_cast<QProcess*>(proc)->kill();
}

extern "C" void qt_process_terminate(qt_process_t proc) {
    static_cast<QProcess*>(proc)->terminate();
}

extern "C" void qt_process_on_finished(qt_process_t proc,
                                        qt_callback_int callback,
                                        long callback_id) {
    // Store callback for manual invocation after our waitpid.
    // We do NOT also connect via QObject::connect because
    // qt_process_wait_for_finished already invokes the callback
    // manually, and the Qt signal would cause a double-fire.
    auto it = s_process_info.find(proc);
    if (it != s_process_info.end()) {
        it->second.finished_cb = callback;
        it->second.finished_cb_id = callback_id;
    } else {
        // Process not yet started — pre-register callback
        s_process_info[proc] = {callback, callback_id, 0, 0};
    }
}

extern "C" void qt_process_on_ready_read(qt_process_t proc,
                                          qt_callback_void callback,
                                          long callback_id) {
    QObject::connect(static_cast<QProcess*>(proc),
                     &QProcess::readyReadStandardOutput,
                     [callback, callback_id]() {
                         callback(callback_id);
                     });
}

extern "C" void qt_process_destroy(qt_process_t proc) {
    auto it = s_process_info.find(proc);
    if (it != s_process_info.end()) {
        if (it->second.pid > 0) {
            qt_untrack_pid(it->second.pid);
            qt_restore_sigchld_handler();
        }
        s_process_info.erase(it);
    }
    delete static_cast<QProcess*>(proc);
}

// ============================================================
// Phase 15: QWizard / QWizardPage
// ============================================================

extern "C" qt_wizard_t qt_wizard_create(qt_widget_t parent) {
    auto* p = parent ? static_cast<QWidget*>(parent) : nullptr;
    return static_cast<void*>(new QWizard(p));
}

extern "C" int qt_wizard_add_page(qt_wizard_t wiz, qt_wizard_page_t page) {
    return static_cast<QWizard*>(wiz)->addPage(
        static_cast<QWizardPage*>(page));
}

extern "C" void qt_wizard_set_start_id(qt_wizard_t wiz, int id) {
    static_cast<QWizard*>(wiz)->setStartId(id);
}

extern "C" int qt_wizard_current_id(qt_wizard_t wiz) {
    return static_cast<QWizard*>(wiz)->currentId();
}

extern "C" void qt_wizard_set_title(qt_wizard_t wiz, const char* title) {
    static_cast<QWizard*>(wiz)->setWindowTitle(
        QString::fromUtf8(title));
}

extern "C" int qt_wizard_exec(qt_wizard_t wiz) {
    return static_cast<QWizard*>(wiz)->exec();
}

extern "C" qt_wizard_page_t qt_wizard_page_create(qt_widget_t parent) {
    auto* p = parent ? static_cast<QWidget*>(parent) : nullptr;
    return static_cast<void*>(new QWizardPage(p));
}

extern "C" void qt_wizard_page_set_title(qt_wizard_page_t page,
                                          const char* title) {
    static_cast<QWizardPage*>(page)->setTitle(
        QString::fromUtf8(title));
}

extern "C" void qt_wizard_page_set_subtitle(qt_wizard_page_t page,
                                             const char* subtitle) {
    static_cast<QWizardPage*>(page)->setSubTitle(
        QString::fromUtf8(subtitle));
}

extern "C" void qt_wizard_page_set_layout(qt_wizard_page_t page,
                                           qt_layout_t layout) {
    static_cast<QWizardPage*>(page)->setLayout(
        static_cast<QLayout*>(layout));
}

extern "C" void qt_wizard_on_current_changed(qt_wizard_t wiz,
                                              qt_callback_int callback,
                                              long callback_id) {
    QObject::connect(static_cast<QWizard*>(wiz),
                     &QWizard::currentIdChanged,
                     [callback, callback_id](int id) {
                         callback(callback_id, id);
                     });
}

// ============================================================
// Phase 15: QMdiArea / QMdiSubWindow
// ============================================================

extern "C" qt_mdi_area_t qt_mdi_area_create(qt_widget_t parent) {
    auto* p = parent ? static_cast<QWidget*>(parent) : nullptr;
    return static_cast<void*>(new QMdiArea(p));
}

extern "C" qt_mdi_sub_window_t qt_mdi_area_add_sub_window(qt_mdi_area_t area,
                                                            qt_widget_t widget) {
    return static_cast<void*>(
        static_cast<QMdiArea*>(area)->addSubWindow(
            static_cast<QWidget*>(widget)));
}

extern "C" void qt_mdi_area_remove_sub_window(qt_mdi_area_t area,
                                               qt_mdi_sub_window_t sub) {
    static_cast<QMdiArea*>(area)->removeSubWindow(
        static_cast<QMdiSubWindow*>(sub));
}

extern "C" qt_mdi_sub_window_t qt_mdi_area_active_sub_window(qt_mdi_area_t area) {
    return static_cast<void*>(
        static_cast<QMdiArea*>(area)->activeSubWindow());
}

extern "C" int qt_mdi_area_sub_window_count(qt_mdi_area_t area) {
    return static_cast<QMdiArea*>(area)->subWindowList().size();
}

extern "C" void qt_mdi_area_cascade(qt_mdi_area_t area) {
    static_cast<QMdiArea*>(area)->cascadeSubWindows();
}

extern "C" void qt_mdi_area_tile(qt_mdi_area_t area) {
    static_cast<QMdiArea*>(area)->tileSubWindows();
}

extern "C" void qt_mdi_area_set_view_mode(qt_mdi_area_t area, int mode) {
    static_cast<QMdiArea*>(area)->setViewMode(
        static_cast<QMdiArea::ViewMode>(mode));
}

extern "C" void qt_mdi_sub_window_set_title(qt_mdi_sub_window_t sub,
                                             const char* title) {
    static_cast<QMdiSubWindow*>(sub)->setWindowTitle(
        QString::fromUtf8(title));
}

extern "C" void qt_mdi_area_on_sub_window_activated(qt_mdi_area_t area,
                                                     qt_callback_void callback,
                                                     long callback_id) {
    QObject::connect(static_cast<QMdiArea*>(area),
                     &QMdiArea::subWindowActivated,
                     [callback, callback_id](QMdiSubWindow*) {
                         callback(callback_id);
                     });
}

// ============================================================
// Phase 16: QDial
// ============================================================

extern "C" qt_dial_t qt_dial_create(qt_widget_t parent) {
    auto* p = parent ? static_cast<QWidget*>(parent) : nullptr;
    return static_cast<void*>(new QDial(p));
}

extern "C" void qt_dial_set_value(qt_dial_t d, int val) {
    static_cast<QDial*>(d)->setValue(val);
}

extern "C" int qt_dial_value(qt_dial_t d) {
    return static_cast<QDial*>(d)->value();
}

extern "C" void qt_dial_set_range(qt_dial_t d, int min, int max) {
    static_cast<QDial*>(d)->setRange(min, max);
}

extern "C" void qt_dial_set_notches_visible(qt_dial_t d, int visible) {
    static_cast<QDial*>(d)->setNotchesVisible(visible != 0);
}

extern "C" void qt_dial_set_wrapping(qt_dial_t d, int wrap) {
    static_cast<QDial*>(d)->setWrapping(wrap != 0);
}

extern "C" void qt_dial_on_value_changed(qt_dial_t d,
                                          qt_callback_int callback,
                                          long callback_id) {
    QObject::connect(static_cast<QDial*>(d),
                     &QDial::valueChanged,
                     [callback, callback_id](int value) {
                         callback(callback_id, value);
                     });
}

// ============================================================
// Phase 16: QLCDNumber
// ============================================================

extern "C" qt_lcd_t qt_lcd_create(int digits, qt_widget_t parent) {
    auto* p = parent ? static_cast<QWidget*>(parent) : nullptr;
    return static_cast<void*>(new QLCDNumber(digits, p));
}

extern "C" void qt_lcd_display_int(qt_lcd_t lcd, int value) {
    static_cast<QLCDNumber*>(lcd)->display(value);
}

extern "C" void qt_lcd_display_double(qt_lcd_t lcd, double value) {
    static_cast<QLCDNumber*>(lcd)->display(value);
}

extern "C" void qt_lcd_display_string(qt_lcd_t lcd, const char* text) {
    static_cast<QLCDNumber*>(lcd)->display(QString::fromUtf8(text));
}

extern "C" void qt_lcd_set_mode(qt_lcd_t lcd, int mode) {
    static_cast<QLCDNumber*>(lcd)->setMode(
        static_cast<QLCDNumber::Mode>(mode));
}

extern "C" void qt_lcd_set_segment_style(qt_lcd_t lcd, int style) {
    static_cast<QLCDNumber*>(lcd)->setSegmentStyle(
        static_cast<QLCDNumber::SegmentStyle>(style));
}

// ============================================================
// Phase 16: QToolBox
// ============================================================

extern "C" qt_tool_box_t qt_tool_box_create(qt_widget_t parent) {
    auto* p = parent ? static_cast<QWidget*>(parent) : nullptr;
    return static_cast<void*>(new QToolBox(p));
}

extern "C" int qt_tool_box_add_item(qt_tool_box_t tb, qt_widget_t widget,
                                     const char* text) {
    return static_cast<QToolBox*>(tb)->addItem(
        static_cast<QWidget*>(widget), QString::fromUtf8(text));
}

extern "C" void qt_tool_box_set_current_index(qt_tool_box_t tb, int idx) {
    static_cast<QToolBox*>(tb)->setCurrentIndex(idx);
}

extern "C" int qt_tool_box_current_index(qt_tool_box_t tb) {
    return static_cast<QToolBox*>(tb)->currentIndex();
}

extern "C" int qt_tool_box_count(qt_tool_box_t tb) {
    return static_cast<QToolBox*>(tb)->count();
}

extern "C" void qt_tool_box_set_item_text(qt_tool_box_t tb, int idx,
                                           const char* text) {
    static_cast<QToolBox*>(tb)->setItemText(idx, QString::fromUtf8(text));
}

extern "C" void qt_tool_box_on_current_changed(qt_tool_box_t tb,
                                                qt_callback_int callback,
                                                long callback_id) {
    QObject::connect(static_cast<QToolBox*>(tb),
                     &QToolBox::currentChanged,
                     [callback, callback_id](int idx) {
                         callback(callback_id, idx);
                     });
}

// ============================================================
// Phase 16: QUndoStack / QUndoCommand
// ============================================================

// Custom QUndoCommand subclass that calls Scheme callbacks for undo/redo.
// Note: QUndoStack::push() calls redo() immediately — this is by design.
class SchemeUndoCommand : public QUndoCommand {
public:
    SchemeUndoCommand(const QString& text,
                      qt_callback_void undo_cb, long undo_id,
                      qt_callback_void redo_cb, long redo_id,
                      qt_callback_void cleanup_cb, long cleanup_id)
        : QUndoCommand(text)
        , m_undo_cb(undo_cb), m_undo_id(undo_id)
        , m_redo_cb(redo_cb), m_redo_id(redo_id)
        , m_cleanup_cb(cleanup_cb), m_cleanup_id(cleanup_id) {}

    ~SchemeUndoCommand() override {
        if (m_cleanup_cb) m_cleanup_cb(m_cleanup_id);
    }

    void undo() override {
        if (m_undo_cb) m_undo_cb(m_undo_id);
    }

    void redo() override {
        if (m_redo_cb) m_redo_cb(m_redo_id);
    }

private:
    qt_callback_void m_undo_cb;
    long m_undo_id;
    qt_callback_void m_redo_cb;
    long m_redo_id;
    qt_callback_void m_cleanup_cb;
    long m_cleanup_id;
};

extern "C" qt_undo_stack_t qt_undo_stack_create(qt_widget_t parent) {
    auto* p = parent ? static_cast<QObject*>(static_cast<QWidget*>(parent))
                     : nullptr;
    return static_cast<void*>(new QUndoStack(p));
}

extern "C" void qt_undo_stack_push(qt_undo_stack_t stack, const char* text,
                                    qt_callback_void undo_cb, long undo_id,
                                    qt_callback_void redo_cb, long redo_id,
                                    qt_callback_void cleanup_cb, long cleanup_id) {
    auto* s = static_cast<QUndoStack*>(stack);
    auto* cmd = new SchemeUndoCommand(QString::fromUtf8(text),
                                      undo_cb, undo_id, redo_cb, redo_id,
                                      cleanup_cb, cleanup_id);
    s->push(cmd);  // QUndoStack takes ownership
}

extern "C" void qt_undo_stack_undo(qt_undo_stack_t stack) {
    static_cast<QUndoStack*>(stack)->undo();
}

extern "C" void qt_undo_stack_redo(qt_undo_stack_t stack) {
    static_cast<QUndoStack*>(stack)->redo();
}

extern "C" int qt_undo_stack_can_undo(qt_undo_stack_t stack) {
    return static_cast<QUndoStack*>(stack)->canUndo() ? 1 : 0;
}

extern "C" int qt_undo_stack_can_redo(qt_undo_stack_t stack) {
    return static_cast<QUndoStack*>(stack)->canRedo() ? 1 : 0;
}

extern "C" const char* qt_undo_stack_undo_text(qt_undo_stack_t stack) {
    s_return_buf = static_cast<QUndoStack*>(stack)
                       ->undoText().toUtf8().toStdString();
    return s_return_buf.c_str();
}

extern "C" const char* qt_undo_stack_redo_text(qt_undo_stack_t stack) {
    s_return_buf = static_cast<QUndoStack*>(stack)
                       ->redoText().toUtf8().toStdString();
    return s_return_buf.c_str();
}

extern "C" void qt_undo_stack_clear(qt_undo_stack_t stack) {
    static_cast<QUndoStack*>(stack)->clear();
}

extern "C" qt_action_t qt_undo_stack_create_undo_action(qt_undo_stack_t stack,
                                                          qt_widget_t parent) {
    auto* p = parent ? static_cast<QObject*>(static_cast<QWidget*>(parent))
                     : nullptr;
    return static_cast<void*>(
        static_cast<QUndoStack*>(stack)->createUndoAction(p));
}

extern "C" qt_action_t qt_undo_stack_create_redo_action(qt_undo_stack_t stack,
                                                          qt_widget_t parent) {
    auto* p = parent ? static_cast<QObject*>(static_cast<QWidget*>(parent))
                     : nullptr;
    return static_cast<void*>(
        static_cast<QUndoStack*>(stack)->createRedoAction(p));
}

extern "C" void qt_undo_stack_destroy(qt_undo_stack_t stack) {
    delete static_cast<QUndoStack*>(stack);
}

// ============================================================
// Phase 16: QFileSystemModel
// ============================================================

extern "C" qt_file_system_model_t qt_file_system_model_create(qt_widget_t parent) {
    auto* p = parent ? static_cast<QObject*>(static_cast<QWidget*>(parent))
                     : nullptr;
    return static_cast<void*>(new QFileSystemModel(p));
}

extern "C" void qt_file_system_model_set_root_path(qt_file_system_model_t model,
                                                     const char* path) {
    static_cast<QFileSystemModel*>(model)->setRootPath(
        QString::fromUtf8(path));
}

extern "C" void qt_file_system_model_set_filter(qt_file_system_model_t model,
                                                  int filters) {
    static_cast<QFileSystemModel*>(model)->setFilter(
        static_cast<QDir::Filters>(filters));
}

extern "C" void qt_file_system_model_set_name_filters(qt_file_system_model_t model,
                                                        const char* patterns) {
    QStringList filters;
    if (patterns && patterns[0]) {
        filters = QString::fromUtf8(patterns).split(
            QChar('\n'), Qt::SkipEmptyParts);
    }
    static_cast<QFileSystemModel*>(model)->setNameFilters(filters);
}

extern "C" const char* qt_file_system_model_file_path(qt_file_system_model_t model,
                                                        int row, int column) {
    auto* m = static_cast<QFileSystemModel*>(model);
    QModelIndex idx = m->index(row, column, m->index(m->rootPath()));
    s_return_buf = m->filePath(idx).toUtf8().toStdString();
    return s_return_buf.c_str();
}

extern "C" void qt_tree_view_set_file_system_root(qt_widget_t view,
                                                    qt_file_system_model_t model,
                                                    const char* path) {
    auto* tv = static_cast<QTreeView*>(view);
    auto* m = static_cast<QFileSystemModel*>(model);
    tv->setModel(m);
    tv->setRootIndex(m->index(QString::fromUtf8(path)));
}

extern "C" void qt_file_system_model_destroy(qt_file_system_model_t model) {
    delete static_cast<QFileSystemModel*>(model);
}

// ============================================================
// Signal disconnect
// ============================================================

extern "C" void qt_disconnect_all(qt_widget_t obj) {
    if (obj) {
        static_cast<QObject*>(obj)->disconnect();
    }
}
