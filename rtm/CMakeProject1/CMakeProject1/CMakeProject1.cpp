// CMakeProject1.cpp : Defines the entry point for the application.
//
#include <iostream>
#include <cstdlib>
#include <map>
#include "CMakeProject1.h"
#include "c:\Users\jolyon\source\repos\CMakeProject1\out\build\x64-debug\vcpkg_installed\x64-windows\include\RtMidi.h"

using namespace std;

int main()
{
	cout << "Hello CMake." << endl;

    // Create an api map.
    std::map<int, std::string> apiMap;
    apiMap[RtMidi::MACOSX_CORE] = "OS-X CoreMIDI";
    apiMap[RtMidi::WINDOWS_MM] = "Windows MultiMedia";
    apiMap[RtMidi::UNIX_JACK] = "Jack Client";
    apiMap[RtMidi::LINUX_ALSA] = "Linux ALSA";
    apiMap[RtMidi::RTMIDI_DUMMY] = "RtMidi Dummy";

    std::vector< RtMidi::Api > apis;
    RtMidi::getCompiledApi(apis);

    std::cout << "\nCompiled APIs:\n";
    for (unsigned int i = 0; i < apis.size(); i++)
        std::cout << "  " << apiMap[apis[i]] << std::endl;
    std::cout << std::endl;

    for (unsigned int i = 0; i < apis.size(); i++) {
        std::cout << "Probing with API " << apiMap[apis[i]] << std::endl;

        RtMidiIn* midiin = 0;
        RtMidiOut* midiout = 0;

        try {

            // RtMidiIn constructor ... exception possible
            midiin = new RtMidiIn(apis[i]);

            std::cout << "\nCurrent input API: " << apiMap[midiin->getCurrentApi()] << std::endl;

            // Check inputs.
            unsigned int nPorts = midiin->getPortCount();
            std::cout << "\nThere are " << nPorts << " MIDI input sources available.\n";

            for (unsigned i = 0; i < nPorts; i++) {
                std::string portName = midiin->getPortName(i);
                std::cout << "  Input Port #" << i << ": " << portName << '\n';
            }

            // RtMidiOut constructor ... exception possible
            midiout = new RtMidiOut(apis[i]);

            std::cout << "\nCurrent output API: " << apiMap[midiout->getCurrentApi()] << std::endl;

            // Check outputs.
            nPorts = midiout->getPortCount();
            std::cout << "\nThere are " << nPorts << " MIDI output ports available.\n";

            for (unsigned i = 0; i < nPorts; i++) {
                std::string portName = midiout->getPortName(i);
                std::cout << "  Output Port #" << i << ": " << portName << std::endl;
            }
            std::cout << std::endl;

        }
        catch (RtMidiError& error) {
            error.printMessage();
        }

        delete midiin;
        delete midiout;
    }

    return 0;
	
	return 0;
}
