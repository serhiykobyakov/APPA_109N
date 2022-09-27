## Free Pascal and Python units for APPA 109N (107N) multimeter

### What it does

You can connect and use use your APPA 109N (107N) multimeter in your projects using appropriate USB/RS232 optical adapter cable and this software.

The unit can acquire the actual reading and calculate the uncertainty. 

The software has been tested for errors, stability and speed (bugs free is not guaranteed, see the licence).


### Uncertainties
The uncertainty of the measurement is a sum of it's random and systematic part. It is defined by the manufacturer and described in the manual as follows:

uncertainty = n% * reading + N * resolution

If we measure a one separate value - it's full uncertainty (3 sigma, 100% confidence interval) will be the sum of both parts.


### Supported platforms
Linux


### Run

You can change the multimeter modes directly on the multimeter. Software just reads the data and have no control over the device.

If the multimeter is switched off by itself (power saving, 45 min) the program will ask you to switch it on. If you want the multimeter to run forever (without auto switch off) just switch it on while holding the blue button.

### Contact
For reporting [bugs, suggestions, patches](https://github.com/serhiykobyakov/APPA_109N_FPC/issues)

### License
The project is licensed under the [MIT license](https://github.com/serhiykobyakov/APPA_109N_FPC/blob/main/LICENSE)
