#ifndef PARTICLECANVAS_H
#define PARTICLECANVAS_H

#include <QWidget>
#include <QTimer>

class ParticleCanvas : public QWidget
{
    Q_OBJECT
private:
    QTimer* m_timer;

public:
    explicit ParticleCanvas(QWidget *parent = nullptr);

    void paintEvent(QPaintEvent *);

    void runSimulation();

signals:


public slots:

    void on_timer_update();


};

#endif // PARTICLECANVAS_H
