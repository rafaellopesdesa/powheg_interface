#!/usr/bin/env python

import os, sys, re

def main(argv):
    input = open(argv[1], 'r')
    qcd = open(argv[2], 'w')
    qed = open(argv[3], 'w')

    line = ''
    
    init = None    
    while init is None:
        line = input.readline()
        init = re.search('</init>', line)
        qcd.write(line)
        qed.write(line)
    buffer = []
    finish = None
    while finish is None:
        event = None
        isQED = None
        while event is None:
            line = input.readline()
            buffer.append(line)
            event = re.search('</event>', line)
            if isQED is None:
                isQED = re.search(' 22 ', line)
        if isQED is None:
            for line in buffer:
                qcd.write(line)
        else:
            for line in buffer:
                qed.write(line)
        buffer = []
        line = input.readline()
        buffer.append(line)
        finish = re.search('</LesHouchesEvents>', line)
    qcd.write(line)
    qed.write(line)
    input.close()
    qcd.close()
    qed.close()

    return



'''
The program reads a file formated as pwgevents.lhe and outputs two files. One with QCD as the hardest radiation and the other with QED as hardest radiation.

   ./splitSamples [input] [QCD] [QED]   
'''

if __name__ == "__main__":
    if len(sys.argv) != 4:
        print 'Usage:                               '
        print '   ./splitSamples [input] [QCD] [QED]'
        sys.exit()
    main(sys.argv)
