#include "mainwindow.h"
#include "./ui_mainwindow.h"

#include <QPainter>
#include <QString>

#include "particle_interface.h"

MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent)
    , ui(new Ui::MainWindow)
    , m_nParticles(500)
    , m_minRadius(0.005)
    , m_maxRadius(0.015)
    , m_v0(0.001)
    , m_firstTime(true)
    , m_maxSize(600.0)
{
    // Setup user interface from UI-description

    ui->setupUi(this);

    // Set default values for controls

    updateControls();

    // Create particle system

    createParticleSystem();
}

MainWindow::~MainWindow()
{
    // Delete particle system

    particle_system_destroy();

    // Delete user interface

    delete ui;
}

void MainWindow::updateParams()
{
    // Update parameters from controls

    bool ok;
    m_nParticles = ui->particleSpin->value();

    auto minRadius = ui->minRadiusText->text().toDouble(&ok);
    if (ok) m_minRadius = minRadius;

    auto maxRadius = ui->maxRadiusText->text().toDouble(&ok);
    if (ok) m_maxRadius = maxRadius;

    // Set updated values in the controls

    updateControls();
}

void MainWindow::updateControls()
{
    // Update controls with values

    ui->particleSpin->setValue(m_nParticles);
    ui->minRadiusText->setText(QString::number(m_minRadius));
    ui->maxRadiusText->setText(QString::number(m_maxRadius));
    ui->maxSizeSlider->setValue(int(m_maxSize));
}

void MainWindow::createParticleSystem()
{
    // Stop timer updates

    ui->drawingArea->stopSimulation();

    // If not first time destroy current particle system

    if (!m_firstTime)
        particle_system_destroy();

    // Create particle system

    particle_system_init(m_nParticles, m_minRadius, m_maxRadius, m_v0);

    m_firstTime = false;

    // Start simulation

    ui->drawingArea->runSimulation();
}

void MainWindow::on_actionRun_triggered()
{
    ui->drawingArea->runSimulation();
}


void MainWindow::on_actionStop_triggered()
{
    ui->drawingArea->stopSimulation();
}

void MainWindow::on_maxSizeSlider_valueChanged(int value)
{
    ui->drawingArea->setMaxSize(double(value));
}


void MainWindow::on_actionUpdate_triggered()
{
    // Handling a click on the Update button

    updateParams();
    createParticleSystem();
}

