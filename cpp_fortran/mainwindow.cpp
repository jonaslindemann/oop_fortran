#include "mainwindow.h"
#include "./ui_mainwindow.h"

#include <QPainter>

#include "particle_interface.h"

MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent)
    , ui(new Ui::MainWindow)
{
    ui->setupUi(this);

    particle_system_init(500);
}

MainWindow::~MainWindow()
{
    particle_system_destroy();
    delete ui;
}


void MainWindow::on_actionRun_triggered()
{
    ui->drawingArea->runSimulation();
}

