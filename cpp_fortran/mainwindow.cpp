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
    ui->setupUi(this);

    updateControls();
    createParticleSystem();
}

MainWindow::~MainWindow()
{
    particle_system_destroy();
    delete ui;
}

void MainWindow::updateParams()
{
    m_nParticles = ui->particleSpin->value();
    m_minRadius = ui->minRadiusText->text().toDouble();
    m_maxRadius = ui->maxRadiusText->text().toDouble();
}

void MainWindow::updateControls()
{
    ui->particleSpin->setValue(m_nParticles);
    ui->minRadiusText->setText(QString::number(m_minRadius));
    ui->maxRadiusText->setText(QString::number(m_maxRadius));
    ui->maxSizeSlider->setValue(int(m_maxSize));
}

void MainWindow::createParticleSystem()
{
    ui->drawingArea->stopSimulation();

    if (!m_firstTime)
        particle_system_destroy();

    particle_system_init(m_nParticles, m_minRadius, m_maxRadius, m_v0);

    m_firstTime = false;

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


void MainWindow::on_updateButton_clicked()
{
    updateParams();
    createParticleSystem();
}


void MainWindow::on_maxSizeSlider_valueChanged(int value)
{
    ui->drawingArea->setMaxSize(double(value));
}

