"""

Title: AstroImageJ photometry plot
Description: see below.
Author: Maksym Pyatnytskyy
Email: mpyat2@gmail.com
Date: 2025-07-17
Version: 1.1

License: MIT License

Usage: aijplot.py [-h] [--sat-lim SAT_LIM] filename [output]

Required packages: astropy, pandas, numpy, matplotlib, base64, argparse, io, sys

"""

DESCRIPTION = \
"""The script creates an HTML file with the light curve plots"""

###############################################################################

MAX_APERTURES = 1000
N_SIGMA = 5
N_CLIP = 5

###############################################################################

def hr_deg_to_sexagesimal(ra_hr, dec_deg):
    from astropy.coordinates import Angle
    import astropy.units as u
    
    ra = Angle(ra_hr, unit=u.hour).to_string(unit=u.hour, sep=':', precision=2, pad=True)
    dec = Angle(dec_deg, unit=u.deg).to_string(unit=u.deg, sep=':', precision=1, alwayssign=True, pad=True)
    return ra, dec

def median_clip_outliners(data1, mag_col):
    # exclude outliers
    median_mag = data1[mag_col].mean()
    std_mag = data1[mag_col].std()
    lower_bound = median_mag - N_SIGMA * std_mag
    upper_bound = median_mag + N_SIGMA * std_mag
    return data1[(data1[mag_col] >= lower_bound) & (data1[mag_col] <= upper_bound)]

def main(aij_photometry_file, output_file, saturation_limit):
    import pandas as pd
    import numpy as np
    import matplotlib.pyplot as plt
    import io
    import base64

    data_raw = pd.read_csv(aij_photometry_file, sep='\t', index_col='slice')

    with open(output_file, "w") as f:
        f.write("<html><body>\n")
        f.write("<h2>Photometry report</h2>\n")
        f.write("<hr>\n")
    
    for i in range(MAX_APERTURES):
        aperture_n = i + 1
        time_col = 'J.D.-2400000'
        ra_col = 'RA_T' + str(aperture_n)
        dec_col = 'DEC_T' + str(aperture_n)
        mag_col = 'Source_AMag_T' + str(aperture_n)
        #err_col = 'Source_AMag_Err_T' + str(aperture_n)
        peak_col = 'Peak_T' + str(aperture_n)
        
        if mag_col in data_raw.columns:
            # remove NaN, Inf
            data_clean = data_raw[np.isfinite(data_raw[mag_col])]

            # exclude outliers
            for i in range(N_CLIP):
                data_clean = median_clip_outliners(data_clean, mag_col)
            
            ra_mean = data_clean[ra_col].mean()
            dec_mean = data_clean[dec_col].mean()
            ra_str, dec_str = hr_deg_to_sexagesimal(ra_mean, dec_mean)
            #ra_str, dec_str = str(ra_mean), str(dec_mean)
            object_name = ra_str + ' ' + dec_str
            object_name2 = 'Object T' + str(aperture_n) + ': ' + object_name;
            print(object_name2)
           
            peak_max = data_clean[peak_col].max()
            is_saturated = saturation_limit > 0 and peak_max >= saturation_limit
            if is_saturated:
                print('**** SATURATED ****')
            
            time = data_clean[time_col]
            mag = data_clean[mag_col]
            #err = data_clean[err_col]            
            
            fig, ax = plt.subplots()
            ax.scatter(time, mag)
            ax.set_xlabel(time_col)            
            ax.set_ylabel('Magnitude')
            plt.gca().invert_yaxis()
            plt.suptitle(object_name)
            if is_saturated:
                plt.title('**** SATURATED ****', color='red')
            #plt.show()
            #print()
            #print()
            
            # Save figure to buffer
            buf = io.BytesIO()
            fig.savefig(buf, format='png', bbox_inches='tight')
            buf.seek(0)
            encoded = base64.b64encode(buf.read()).decode('utf-8')
            buf.close()
            plt.close(fig)
            with open(output_file, "a") as f:
                f.write(f"<p>{object_name2}</p>\n")
                if is_saturated:
                    f.write('<p>**** SATURATED ****<p>\n')
                f.write(f"<img src='data:image/png;base64,{encoded}'><br><br>\n")
                f.write("<hr>\n")
                #f.flush()

    with open(output_file, "a") as f:
        f.write("<p>End of file</p>\n")
        f.write("\n</body></html>")                


def parse_args():
    import argparse
    parser = argparse.ArgumentParser(description=DESCRIPTION)
    # Required positional argument: input file
    parser.add_argument("filename", type=str, help="Path to the input photometry table (AstroImageJ format with magnitudes)")
    # Optional positional argument: output file with default
    parser.add_argument("output", nargs="?", default="Photometry.html",
                        help="Output file name. Default: Photometry.html")
    # Optional named argument: Saturation limit
    parser.add_argument("--sat-lim", type=float, default=-1,
                        help="Saturation limit. Ignored if it is <= 0. Default: -1 (i.e., ignored)")
    return parser.parse_args()


import sys

if __name__ == "__main__":
    try:
        args = parse_args()
        #print(args)
        #print(f"Input file: {args.filename}")
        #print(f"Output file: {args.output}")
        #print(f"Saturation: {args.sat_lim}")
        main(args.filename, args.output, args.sat_lim)
    except Exception as e:
        print(f"Fatal Error: {e}.")
        print("Press ENTER to continue:")
        input("")
    finally:
        print("End")
        sys.exit()
