#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>

QT_BEGIN_NAMESPACE
namespace Ui { class MainWindow; }
QT_END_NAMESPACE

class MainWindow : public QMainWindow
{
    Q_OBJECT

private:
    int m_nParticles;
    double m_minRadius;
    double m_maxRadius;
    double m_v0;
    bool m_firstTime;
    double m_maxSize;

public:
    MainWindow(QWidget *parent = nullptr);
    ~MainWindow();

    void updateParams();
    void updateControls();

    void createParticleSystem();

private slots:
    void on_actionRun_triggered();
    void on_actionStop_triggered();
    void on_maxSizeSlider_valueChanged(int value);
    void on_actionUpdate_triggered();

private:
    Ui::MainWindow *ui;
};
#endif // MAINWINDOW_H
