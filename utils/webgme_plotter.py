#!/usr/bin/python
# WebGME ROSMOD Trace Log Plotter
# Author: Pranav Srinivas Kumar
# Date: 2016.05.18

import os
import sys
import matplotlib as mpl
import matplotlib.pyplot as plt
from mpl_toolkits.axes_grid.anchored_artists import AnchoredText
import pylab
import argparse

class ROSMOD_Operation():
    def __init__(self):
        self.node = ""
        self.component_instance = ""
        self.name = ""
        self.enqueue_time = 0.0
        self.completion_time = 0.0
        self.deadline = 0.0
        self.exec_time = 0.0

class ROSMOD_Log_Plotter():
    def __init__(self):
        self.node = ""
        self.component_instance = ""
        self.operations = []
        self.unique_operations = []
        self.colors = ['b', 'g', 'r', 'k']
        self.opn_max = 0
        self.time_unit = ""

    def parse(self, log, unit):
        log_contents = ""

        with open (log, "r") as logs:
            log_contents += logs.read()
        node_name = log.split('.')[0]
        component_instance = log.split('.')[1]            

        if unit == 's':
            divide = 1000000000.0            
        elif unit == 'ms':
            divide = 1000000.0
        elif unit == 'us':
            divide = 1000.0                        
        elif unit == 'ns':
            divide = 1.0                                    

        else:
            print "ERROR: Provided unit is invalid! Correct Values = (s, ms, us, ns)"
            return
        self.time_unit = unit
        for line in log_contents.split('\n'):
            if 'init_timer_operation' not in line and '===' not in line:
                split_line = line.split('::')
                values = split_line[-1].split(', ')
                if len(values) == 6:
                    new_operation = ROSMOD_Operation()
                    new_operation.name = values[0]
                    if new_operation.name not in self.unique_operations:
                        self.unique_operations.append(new_operation.name)
                    new_operation.node = node_name
                    new_operation.component_instance = component_instance
                    new_operation.enqueue_time = float(values[1].split('.')[0] + values[1].split('.')[-1])/divide
                    new_operation.completion_time = float(values[3].split('.')[0] + values[3].split('.')[-1])/divide
                    new_operation.exec_time = float(values[4].split('.')[0] + values[4].split('.')[-1])/divide
                    new_operation.deadline = float(values[5].split('.')[0] + values[5].split('.')[-1])/divide
                    self.operations.append(new_operation)

    def plot(self, opn_max, combined, directory):
        index = 0
        subnum = 0
        fig = plt.figure()
        x_min = 0.0
        x_max = 0
        for call in self.unique_operations:
            count = 0
            calls = [c for c in self.operations if c.name == call]
            x_axis_lim_start = 0.0
            x_axis_lim_end = 0.0
            for instance in calls:
                if opn_max == 'all' or count <= int(opn_max):
                    if x_axis_lim_start == 0.0:
                        x_axis_lim_start = instance.enqueue_time
                    count = count + 1
                    x_axis_lim_end = instance.completion_time
                else:
                    x_axis_lim_end = instance.completion_time
                    break   
            if x_min == 0.0:
                x_min = x_axis_lim_start
            elif x_axis_lim_start < x_min:
                x_min = x_axis_lim_start
            if x_axis_lim_end > x_max:
                x_max = x_axis_lim_end

        for call in self.unique_operations:
            if subnum == 0:
                ax = fig.add_subplot(111)
            if subnum > 0:
                n = len(fig.axes)
                for i in range(n):
                    fig.axes[i].change_geometry(n+1, 1, i+1)
                ax = fig.add_subplot(n+1, 1, n+1)
            color = self.colors[index % len(self.colors)]
            index = index + 1
            calls = [c for c in self.operations if c.name == call]
            x_axis = []
            y_axis = []
            deadline = []
            for instance in calls:
                if opn_max == 'all' or count <= int(opn_max):
                    x_axis.extend([instance.enqueue_time, instance.enqueue_time,
                                   instance.completion_time, instance.completion_time])
                    y_axis.extend([0.0, instance.exec_time, instance.exec_time, 0.0])
                    deadline.extend([instance.deadline, instance.deadline, instance.deadline,
                                     instance.deadline])
                else:
                    break
            print y_axis

            exec_plot = plt.plot(x_axis, y_axis, color, label=call)
            at = AnchoredText("Node: " + calls[0].node + "\nComponent Instance: " +\
                              calls[0].component_instance +\
                              "\nOperation Deadline:" + str(instance.deadline) + ' ' +\
                              self.time_unit\
                              + "\nMaximum Execution Time: " +\
                              str(max([instance.exec_time for instance in calls])) + ' ' +\
                              self.time_unit +\
                              "\nAverage Execution Time: " +\
                              str(sum([instance.exec_time\
                                       for instance in calls])/\
                                  len([instance.exec_time for instance in calls])) +\
                              ' ' + self.time_unit,
                              prop=dict(size=8), frameon=True,
                              loc=2,
                          )
            at.patch.set_boxstyle("round,pad=0.,rounding_size=0.5")
            ax.add_artist(at)
            subnum += 1
        
            plt.xlabel('Test Timestamp (' + self.time_unit + ')', fontsize=12)
            plt.ylabel('Execution Time (' + self.time_unit + ')', fontsize=12)
            plt.title('Operation Execution Plot - ' + call, fontsize=12)
            plt.grid(True)
            plt.legend(loc=1,prop={'size':8})
            pylab.xlim([x_min, x_max])
            plt.rcParams.update({'axes.labelsize': 'small'})
        fig = plt.gcf()
        fig.set_size_inches(18.5, 10.5)
        fig.subplots_adjust(hspace=1.0)
        if combined == False:
            if directory != '.':
                plt.show()
                fig.savefig(os.path.join(directory, call + '.png'), dpi=100)
                fig_path = str(os.path.join(directory, call + '.png'))
                print "Plot: " + call + '.png saved at: ' + directory
            else:
                plt.show()                
                fig.savefig(call + '.png', dpi=100)
                fig_path = str(call + '.png')
        else:
            if directory != '.':
                plt.show()
                fig.savefig(os.path.join(directory, 'combined.png'), dpi=100)
                fig_path = str(os.path.join(directory, 'combined.png'))
                print "Plot: " + 'combined.png saved at: ' + directory
            else:
                plt.show()
                fig.savefig('combined.png', dpi=100)
                fig_path = 'combined.png'    

def main():

    parser = argparse.ArgumentParser(description=\
                                     'functionality: Plot Component Operation Execution Behavior',
                                     formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('--log', nargs='?', default='all', help='Name of log file to process')
    parser.add_argument('--num_opn', nargs='?', default='all',
                        help='Number of operation instances to plot')
    parser.add_argument('--unit', nargs='?', default='ms', choices=['s', 'ms', 'us', 'ns'],
                        help='Time unit of the plot')
    parser.add_argument('--save', nargs='?', default='.', help='Path where plots should be saved')
    args = vars(parser.parse_args())

    if args['log'] == 'all':
        logs = [f for f in os.listdir('.') if os.path.isfile(f)]
        log_contents = ""
        for log in logs:
            if "trace.log" in log:
                plotter = ROSMOD_Log_Plotter()
                plotter.parse(log, args['unit'])
                # Provide the number of operation instances to plot
                plotter.plot(args['num_opn'], 
                             True,
                             args['save'])

    else:
        logs = [f for f in os.listdir('.') if os.path.isfile(f)]        
        for log in logs:
            if args['log'] in log:
                plotter = ROSMOD_Log_Plotter()
                plotter.parse(log, args['unit'])
                # Provide the number of operation instances to plot
                plotter.plot(args['num_opn'], 
                             True,
                             args['save'])            
        

if __name__ == "__main__":
    main()
                
