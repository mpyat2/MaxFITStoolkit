"""

Title: Aperture list generator (for AstroImageJ)
Description: see below.
Author: Maksym Pyatnytskyy
Email: mpyat2@gmail.com
Date: 2025-07-09
Version: 1.1

License: MIT License

Usage: apgen.py [-h] [--fwhm FWHM] [--plot] filename [output]

Required packages: astropy, photutils, matplotlib, numpy, argparse, sys

"""

DESCRIPTION = \
"""The script takes an input FITS image (which must contain WCS) and extracts
the coordinates of the star-like sources into an output file."""

from astropy.io import fits
from astropy.wcs import WCS
from astropy.stats import sigma_clipped_stats
from astropy.coordinates import Angle
import astropy.units as u
from photutils.detection import DAOStarFinder
import matplotlib.pyplot as plt
import numpy as np
import argparse
import sys

def deg_to_sexagesimal(ra_deg, dec_deg):
    ra = Angle(ra_deg, unit=u.deg).to_string(unit=u.hour, sep=':', precision=2, pad=True)
    dec = Angle(dec_deg, unit=u.deg).to_string(unit=u.deg, sep=':', precision=1, alwayssign=True, pad=True)
    return ra, dec

def process_image(file_name, output_list, fwhm, plot_image):
    # Load FITS image
    hdu = fits.open(file_name)[0]
    header = hdu.header
    if 'OBJECT' in header:
        object_name = header['OBJECT']
    else:
        object_name = '<Unknown>'
    image_data = hdu.data
    wcs = WCS(hdu.header)
    
    # Estimate background and noise
    mean, median, std = sigma_clipped_stats(image_data, sigma=3.0)
    threshold = median + (5. * std)
    print()
    print('Background Median    =', median)
    print('Background StDev     =', std)
    print('Detection Threshold  =', threshold)
    
    # Detect stars
    daofind = DAOStarFinder(fwhm=fwhm, threshold=threshold)
    sources = daofind(image_data - median)
    
    print()
    print(str(len(sources)) + ' sources found')
    
    # Convert pixel to RA/Dec
    world_coords = wcs.pixel_to_world(sources['xcentroid'], sources['ycentroid'])
    
    # Write to RA/Dec file
    with open(output_list, "w") as f:
        for ra, dec in zip(world_coords.ra.deg, world_coords.dec.deg):
#            star_coords.append((ra, dec))
            ra_str, dec_str = deg_to_sexagesimal(ra, dec)
            f.write(f"{ra_str}, {dec_str}, 0, 1, 99.999\n")
    print()
    print('List of objects coordinates has been created:', output_list)
    
    if plot_image:
        vmin = np.percentile(image_data, 1)  # 1st percentile
        vmax = np.percentile(image_data, 99)  # 99th percentile
        fig = plt.figure(figsize=(6, 6))
        ax = fig.add_subplot(111, projection=wcs)
        im = ax.imshow(image_data, cmap='gray', origin='lower', vmin=vmin, vmax=vmax)
        ax.coords.grid(color='white', ls='dotted')
        ax.coords[0].set_axislabel('Right Ascension')
        ax.coords[1].set_axislabel('Declination')
        plt.colorbar(im, ax=ax, orientation='vertical')
        plt.title('Object: ' + str(object_name))

        # Mark the stars with small circles
        for i in range(len(sources)):
            x = sources['xcentroid'][i]
            y = sources['ycentroid'][i]
            circle = plt.Circle((x, y), radius=fwhm / 2.0, color='red', fill=False)
            plt.gca().add_patch(circle)
        plt.show()

def restricted_float(x):
    try:
        x = float(x)
    except ValueError:
        raise argparse.ArgumentTypeError(f"{x} is not a valid float")
    if x < 1.0 or x > 20.0:
        raise argparse.ArgumentTypeError(f"FWHM must be between 1 and 20 (got {x})")
    return x

def parse_args():
    parser = argparse.ArgumentParser(description=DESCRIPTION)
    # Required positional argument: input file
    parser.add_argument("filename", type=str, help="Path to the input image file")
    # Optional positional argument: output file with default
    parser.add_argument("output", nargs="?", default="radec_list.txt",
                        help="Output file name. Default: radec_list.txt")
    # Optional named argument: FWHM
    parser.add_argument("--fwhm", type=restricted_float, default=4.0,
                        help="Full Width at Half Maximum (FWHM) in pixels (1–20). Default: 4.0")
    # Optional boolean argument: show plot
    parser.add_argument('--plot', action='store_true', default=False,
                        help='Show the image with apertures. Default: do not show)')
    return parser.parse_args()

def main():
    args = parse_args()
    #print(f"Input file: {args.filename}")
    #print(f"Output file: {args.output}")
    #print(f"FWHM: {args.fwhm}")
    #print(f"Plot: {args.plot}")
    process_image(args.filename, args.output, args.fwhm, args.plot)

if __name__ == "__main__":
    try:
        main()
    except Exception as e:
        print(f"Fatal Error: {e}. Press ENTER to continue:")
        input("")
    finally:
        #print("End")
        sys.exit()
