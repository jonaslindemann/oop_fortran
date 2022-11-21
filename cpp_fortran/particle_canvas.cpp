#include "particle_canvas.h"

#include <QPainter>
#include <iostream>

#include "particle_interface.h"

ParticleCanvas::ParticleCanvas(QWidget *parent)
    : QWidget{parent}
{
    m_timer = new QTimer(this);
    connect(m_timer, SIGNAL(timeout()), this, SLOT(on_timer_update()));
}

void ParticleCanvas::paintEvent(QPaintEvent *)
{
    QPainter painter(this);
    painter.setRenderHint(QPainter::Antialiasing, true);
    painter.setPen(Qt::blue);
    painter.setBrush(Qt::black);

    painter.drawRect(0,0,this->width(), this->height());

    auto count = particle_system_particle_count();

    painter.setBrush(Qt::white);
    painter.setPen(Qt::white);

    for (auto i=1; i<=count; i++)
    {
        double x, y, r;
        particle_system_particle(i, &x, &y, &r);
        painter.drawEllipse(x*500, y*500, r*500, r*500);
    }

    std::cout << "redraw()\n";
}

void ParticleCanvas::runSimulation()
{
    m_timer->start();
}

void ParticleCanvas::on_timer_update()
{
    particle_system_run_iteration();
    this->repaint();
}
