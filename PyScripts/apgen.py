from astropy.io import fits
from astropy.wcs import WCS
from photutils.detection import DAOStarFinder
from astropy.stats import sigma_clipped_stats
from astropy.coordinates import Angle
import astropy.units as u
import argparse
import sys
#import numpy as np

#RADEC_LIST = 'radec_list.txt'

def deg_to_sexagesimal(ra_deg, dec_deg):
    ra = Angle(ra_deg, unit=u.deg).to_string(unit=u.hour, sep=':', precision=2, pad=True)
    dec = Angle(dec_deg, unit=u.deg).to_string(unit=u.deg, sep=':', precision=1, alwayssign=True, pad=True)
    return ra, dec

def process_image(file_name, output_list, fwhm):
    # Load FITS image
    hdu = fits.open(file_name)[0]
    data = hdu.data
    wcs = WCS(hdu.header)
    
    # Estimate background and noise
    mean, median, std = sigma_clipped_stats(data, sigma=3.0)
    threshold = median + (5. * std)
    print('Background Median    =', median)
    print('Background StDev     =', std)
    print('Detection Threshold  =', threshold)
    
    # Detect stars
    daofind = DAOStarFinder(fwhm=fwhm, threshold=threshold)
    sources = daofind(data - median)
    
    # Convert pixel to RA/Dec
    world_coords = wcs.pixel_to_world(sources['xcentroid'], sources['ycentroid'])
    
    # Write to RA/Dec file
    with open(output_list, "w") as f:
        for ra, dec in zip(world_coords.ra.deg, world_coords.dec.deg):
            ra_str, dec_str = deg_to_sexagesimal(ra, dec)
            f.write(f"{ra_str}, {dec_str}, 0, 1, 99.999\n")
    print('List of objects coordinates has been created:', output_list)

def restricted_float(x):
    try:
        x = float(x)
    except ValueError:
        raise argparse.ArgumentTypeError(f"{x} is not a valid float")
    if x < 1.0 or x > 20.0:
        raise argparse.ArgumentTypeError(f"FWHM must be between 1 and 20 (got {x})")
    return x

def parse_args():
    parser = argparse.ArgumentParser(description="Process an image file with a specified FWHM.")
    # Required positional argument: input file
    parser.add_argument("filename", type=str, help="Path to the input image file")
    # Optional positional argument: output file with default
    parser.add_argument("output", nargs="?", default="radec_list.txt",
                        help="Output file name (default: radec_list.txt)")
    # Optional named argument: FWHM
    parser.add_argument("--fwhm", type=restricted_float, default=3.0,
                        help="Full Width at Half Maximum (FWHM) in pixels (1–20). Default: 3.0")
    return parser.parse_args()

def main():
    args = parse_args()
    #print(f"Input file: {args.filename}")
    #print(f"Output file: {args.output}")
    #print(f"FWHM: {args.fwhm}")
    process_image(args.filename, args.output, args.fwhm)

if __name__ == "__main__":
    try:
        main()
    except Exception as e:
        print(f"Fatal Error: {e}. Press ENTER to continue:")
        input("")
    finally:
        print("End")
        sys.exit()
