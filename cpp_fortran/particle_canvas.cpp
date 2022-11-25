#include "particle_canvas.h"

#include <QPainter>
#include <iostream>

#include "particle_interface.h"

ParticleCanvas::ParticleCanvas(QWidget *parent)
    : QWidget{parent}
    , m_maxSize(600.0)
{
    // Create timer for repeatly redrawing particle system

    m_timer = new QTimer(this);

    // Connect timer signal to on_timer_update()

    connect(m_timer, SIGNAL(timeout()), this, SLOT(on_timer_update()));
}

void ParticleCanvas::paintEvent(QPaintEvent *)
{
    QPainter painter(this);
    painter.setRenderHint(QPainter::Antialiasing, true);

    // Clear background

    painter.setPen(Qt::blue);
    painter.setBrush(Qt::black);
    painter.drawRect(0,0,this->width(), this->height());

    // Get number of particles

    auto count = particle_system_particle_count();

    painter.setBrush(Qt::white);
    painter.setPen(Qt::white);

    double x, y, r;
    double c;
    double rmin, rmax, v0;

    // Get rmin and rmax parameters

    particle_system_params(&rmin, &rmax, &v0);

    // Loop for drawing all particles

    for (auto i=1; i<=count; i++)
    {
        // Get particle position and size

        particle_system_particle(i, &x, &y, &r);

        // Draw particles

        c = 255.0 * (r - rmin) / (rmax - rmin);
        painter.setBrush((QBrush(QColor(255, 255, int(c)))));
        painter.setPen((QPen(QColor(255, 255, int(c)))));
        painter.drawEllipse(QPointF(width()/2.0 - m_maxSize/2.0 + x*m_maxSize, height()/2.0 -m_maxSize/2.0 + y*m_maxSize), r*m_maxSize, r*m_maxSize);
    }
}

void ParticleCanvas::setMaxSize(double maxSize)
{
    m_maxSize = maxSize;
}

void ParticleCanvas::runSimulation()
{
    // Start timer

    m_timer->start();
}

void ParticleCanvas::stopSimulation()
{
    // Stop timer

    m_timer->stop();
}

void ParticleCanvas::on_timer_update()
{
    particle_system_run_iteration();
    this->repaint();
}
