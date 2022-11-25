#ifndef PARTICLECANVAS_H
#define PARTICLECANVAS_H

#include <QWidget>
#include <QTimer>
#include <QBrush>
#include <QPen>

class ParticleCanvas : public QWidget
{
    Q_OBJECT
private:
    QTimer* m_timer;
    double m_maxSize;

public:
    explicit ParticleCanvas(QWidget *parent = nullptr);

    void paintEvent(QPaintEvent *);

    void setMaxSize(double maxSize);

    void runSimulation();
    void stopSimulation();


signals:


public slots:

    void on_timer_update();
};

#endif // PARTICLECANVAS_H
