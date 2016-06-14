#!/bin/bash

ffmpeg -framerate 1 -pattern_type glob -i '*.png' -i innovations.mp3 -c:v libx264 -r 1 -c:a copy -shortest out.mp4

