## Free Pascal and Python units for APPA 109N (107N) multimeter

### What it does

You can connect and use use your APPA 109N (107N) multimeter in your Free Pascal (Lazarus) projects using appropriate USB/RS232 optical adapter cable and this software.

The unit can acquire the actual multimeter readings and calculate the uncertainties of the reading (the full and the random part, see the description below). The measurement units are also available as strings as well as a measurement range.

The software has been tested for errors, stability and speed (bugs free is not guaranteed, see the licence).

### Uncertainties
The uncertainty of the measurement is a sum of it's random part and the systematic part. It is defined by the manufacturer and described in the manual as follows:

uncertainty = n% * reading + N * resolution

If we measure a one separate value - it's uncertainty will be the sum of both parts, but if we acquire few values at once and we are interested in the relative difference between the measurements - we do not necessary have to consider the systematic part. For example, if we measure temperature dependence of some parameter in order to calculate it's temperature coefficient - the systematic part of the uncertainties can be discarded.

Taking this into account there is an option to get the full uncertainty using function GetUncertaintySI() or just it's random part using GetRandUncertaintySI().


### Supported platforms
Linux


### Run

You may change the modes on the fly while reading the data from multimeter. So be careful - do not change modes in the experiment )))

If the multimeter is switched off automatically the program will ask you to switch it on. If you want the multimeter to run forever (without auto switch off) just switch it on while holding the blue button.

### Contact
For reporting [bugs, suggestions, patches](https://github.com/serhiykobyakov/APPA_109N_FPC/issues)

### License
The project is licensed under the [MIT license](https://github.com/serhiykobyakov/APPA_109N_FPC/blob/main/LICENSE)
