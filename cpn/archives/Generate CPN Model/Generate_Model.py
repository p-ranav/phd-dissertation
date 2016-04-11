import os
outputformat = "html"
from Cheetah.Template import Template

# Global Variables
config_file_name = "config.txt"
model_file_name = ""
number_of_partitions = 0
schedule = []
comp_threads = []
model_schedule = ""
model_threads = ""
ready = []

# Parse config file to obtain:
# (1) Temporal Partition Schedule
# (2) Component Worker Thread information
def parse_config(filename):
    global model_file_name, number_of_partitions, schedule, comp_thread
    f = open(filename, 'rb')
    print "Parsing " + filename + "..."
    for line in f:
        if not '#' in line:

            # Parsing Model File Name
            config_line = line.split("=")
            if len(config_line) == 2:
                model_file_name = config_line[1].strip()
            # Parsing Minor Frames
            config_line = line.split(':')
            config_line[-1] = config_line[-1].strip()
            if len(config_line) == 4:
                schedule.append(config_line)
            if len(config_line) == 5:
                comp_threads.append(config_line)

    # Debug Print Statements
    '''
    # Print Model File Name
    print "Model File Name: ", model_file_name

    # Print Number of Minor Frames
    print "Number of Partitions: ", len(schedule)
    
    # Print Temporal Partition Schedule
    print "Temporal Partition Schedule: ", schedule

    # Print Component Worker Thread Information
    print "Component Worker Threads: ", comp_threads
    '''

def generate_schedule_metadata():
    global model_schedule, ready
    model_schedule = "1`{mf=10,mfc=1,\npl=["
    ready = " 1`[ "
    for i in range(0, len(schedule) - 1):
        model_schedule += "{pn=" + schedule[i][0] + \
                          ", mf_tc=0, mf_mtc=" + schedule[i][2] + \
                          ", mf_pr=" + schedule[i][1] + \
                          ", mf_off=" + schedule[i][3] + \
                          "},\n      "
        ready += "{pn=" + schedule[i][0] + ",ths=[]},\n"
        
    model_schedule += "{pn=" + schedule[i+1][0] + \
                      ", mf_tc=0, mf_mtc=" + schedule[i+1][2] + \
                      ", mf_pr=" + schedule[i+1][1] + \
                      ", mf_off=" + schedule[i+1][3] + \
                      "}]}"
    ready += "      {pn=" + schedule[i+1][0] + ",ths=[]}]" 


def generate_thread_metadata():
    global model_threads, comp_threads
    for i in range(0, len(comp_threads)-1):
        model_threads += "1`{pr=10,t=" + comp_threads[i][0] + \
                         ",p=" + comp_threads[i][1] + \
                         ",pn=" + comp_threads[i][2] + \
                         ",tc=0,mtc=" + comp_threads[i][3] + \
                         ",st=0,et=0,dl=" + comp_threads[i][4] + \
                         "}++\n"
    model_threads += "1`{pr=10,t=" + comp_threads[i+1][0] + \
                     ",p=" + comp_threads[i+1][1] + \
                     ",pn=" + comp_threads[i+1][2] + \
                     ",tc=0,mtc=" + comp_threads[i+1][3] + \
                     ",st=0,et=0,dl=" + comp_threads[i+1][4] + \
                     "}"
    
if __name__ == "__main__":

    # Parse the config file. Resultant lists are stored in schedule and comp_thread
    parse_config(config_file_name)

    # Generate the metadata for the partition schedule
    generate_schedule_metadata()

    # Generate the metadata for component worker threads
    generate_thread_metadata()

    print "Generating " + model_file_name + "..."
    nameSpace = {'partition_schedule': model_schedule, 'ready': ready, 'component_worker_threads': model_threads}
    t = Template(file="cpn_template.tmpl", searchList=[nameSpace])
    #print str(t)
    if os.path.exists(model_file_name):
        os.remove(model_file_name)
        cpn = file(model_file_name, 'w')
    else:
        cpn = file(model_file_name, 'w')
    cpn.write(str(t))
    print "Done!"
    cpn.close()


