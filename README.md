## refereebox/ccs
This is the repository of the @work Referee Box for the Robocup@Work League. 
This referee box is an adaption of the RoCKin CHF (http://rockinrobotchallenge.eu/). 

## Installation
The @work Referee Box can be installed on most Linux distributions. However, some require dependencies being built from source. The currently supported setup is Ubuntu 14.04, with Boost 1.54. For other non-supported distributions and setups see Alternative Installation Hints at the end of this readme.

### Officially Supported Setup: Ubuntu 14.04, Boost 1.54

1. Add [Tim Niemueller's PPA](https://launchpad.net/~timn/+archive/ubuntu/clips):
      
        sudo add-apt-repository ppa:timn/clips
    (Note: This PPA currently only works for Ubuntu 12.04, 12.10 and 14.04.)
    
2. Install the dependencies for both LLSFRB and CFH:
        
        sudo apt-get update
        sudo apt-get install libmodbus-dev libclips-dev clips libclipsmm-dev \
                             protobuf-compiler libprotobuf-dev libprotoc-dev \
                             boost1.54-all-dev libmodbus-dev \
                             libglibmm-2.4-dev libgtkmm-3.0-dev \
                             libncursesw5-dev libyaml-cpp-dev libavahi-client-dev git \
                             libssl-dev libelf-dev mongodb-dev mongodb-clients \
                             mongodb libzmq3-dev libmosquitto-dev

     (Note: Boost 1.54 is specified to avoid causing apt-get broken package problems with ROS. If you are using another version of Boost see Alternative Setup.)

3. Clone this repository:
        
        

4. Build the Central Factory Hub:
        
        cd css
        make

5. Go to Configuration Section before running the referee box.


## Configuration


------------------------------------

## Unsupported Installations
The Referee Box is known to work on several versions of Ubuntu, and ArchLinux. But the combinations of dependencies is very large, and they may cause conflicts. Users with alternative setups are assumed to be aware of their system dependencies. 

### Alternative Installation Hints

Users attempting to use alternative, unsupported systems are doing so without any guarentees. These are just hints.

1. To install the Referee Box, please read and install the prerequisites of the LLSF referee box which are described here:
   
    (Note: You MAY NOT be able to copy and paste from the following link. Ubuntu and Fedora instructions are provided. However, The instructions contain specific package versions, which are not available on all versions of Ubuntu. Search your package manager to find required versions.)
    
    https://trac.fawkesrobotics.org/wiki/LLSFRefBox/Install. 

2. Additionally, the following debian packages need to be installed (if available):
    
        sudo apt-get install libssl-dev libelf-dev mongodb-dev mongodb-clients mongodb libzmq3-dev libmosquitto-dev

3. Older versions of Ubuntu are known to not have all of the above packages. The following may be installed from source if not available:
    - clips 6.30+ (http://sourceforge.net/projects/clipsmm/files/clips)
    - clipsmm 0.3.4+(http://sourceforge.net/projects/clipsmm/files/clipsmm)
    - zeromq 3+ (http://zeromq.org/) 
    - zeromq.hpp (https://github.com/zeromq/cppzmq/blob/master/zmq.hpp) 
            
            cd /usr/local/include/
            sudo wget https://raw.githubusercontent.com/zeromq/cppzmq/master/zmq.hpp
    - boost 1.48+ (http://www.boost.org/) It is STRONGLY recommended to install boost through your package manager, and to choose the version which does not conflict with your other dependencies.
  
4. Clone and build this repository as described above.
