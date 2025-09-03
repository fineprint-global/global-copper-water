[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.17044343.svg)](https://doi.org/10.5281/zenodo.17044343)

-----

### **global-copper-water**

Machine learning for estimating water use in global copper mining.

-----

### **1. System Requirements**

#### **Software Dependencies**

This project requires R. All specific R package dependencies and their version numbers are managed automatically using the `renv` package to ensure a fully reproducible environment.

#### **Tested Environment**

The code has been tested on using R version 4.4.1 (2024-06-14) running on Ubuntu 24.04.2 LTS x86_64-pc-linux-gnu (64-bit). An environment with 16G RAM is sufficient to run the code with other software in use simultaneously (e.g. browser, code editor, email client provider). No specific non-standard hardware is required.

-----

### **2. Installation Guide**

The easiest way to install all dependencies is to use `renv`.

1.  **Clone the Repository**:

    ```bash
    git clone https://github.com/fineprint-global/global-copper-water.git
    cd global-copper-water
    ```

2.  **Install `renv`**: Open R and install the `renv` package.

    ```r
    install.packages("renv")
    ```

3.  **Restore the Environment**: Run the following command from the R console. This will read the `renv.lock` file and automatically download and install the exact versions of all packages required by the project.

    ```r
    renv::restore()
    ```

4. **Typical Install Time:** Installation of all packages on a normal desktop computer with a stable internet connection should take approximately 10-20 minutes.

-----

### **3. Data requirements**

To run the code on your own data, you must provide a file with a similar structure to the original `data/copper_data_20250227.xlsx` file.

1.  **Prepare your Data**:

      * Ensure your data file is in an Excel format (`.xlsx`) and contains the following columns for each mining site:
          * **`Longitude`**: The longitude of the site.
          * **`Latitude`**: The latitude of the site.
          * **`REG_TOP_20`**: A categorical variable for the top 20 regions. Possible values include: "AFR_20", "EUR_20", "SAM_20", "ASI_20", "NOR_20", "OCE_20", or NA.
          * **`mine`**: The name of the mine. The script expects NA for unknown values.
          * **`snl_id`**: A unique ID for the site. The script expects NA for unknown values.
          * **`country`**: The country of the mine.
          * **`country_code`**: The two-letter country code.
          * **`region`**: The region of the mine.
          * **`cumulative_production`**: The cumulative production of the mine.
          * **`average_production`**: The average production of the mine.
          * **`by-prod-group\ 2`**: Byproduct group. Possible values include: "CuCu", "CuZn", "CuCo", "CuAu", "CuAg", "CuPb", "CuNN".
          * **`mine_type_combined`**: The type of mine. Possible values include: "Underground", "Open Pit", "Combined".
          * **`Ore Body Group`**: The ore body group. Possible values include: "NnNn", "PoPo", "SeHo", "Skarn", "IoCu", "UnCo", "CuFe".
          * **`Process route`**: The processing route. Possible values include: "Other", "pyro", "hydro", or "pyro/hydro".
          * **`Ore Grade_combined`**: The ore grade.
      * For the time-series data, ensure your file includes columns for each year from **2015 to 2019** with the following naming convention:
          * **`prod_[Year]`**: e.g., `prod_2015` (for production).
          * **`ToWa_[Year]`**: e.g., `ToWa_2015` (for total water).
          * **`RaWa_[Year]`**: e.g., `RaWa_2015` (for raw water).
      * Place your data file in the `./data/` folder and update the file path in `00-prepare-data.R`.

2.  **Manual Data Downloads Required**:

    * Some data files used in this project cannot be downloaded automatically from the scripts due to the host server requiring a login. You must manually download these files and place them in the correct directory for the scripts to run without errors.

        * **Freshwater Availability Trend Data**: Download and place `sdei-trends-freshwater-availability-grace-2002-2016-geotiff.zip` in the `./data/` folder. URL: `https://sedac.ciesin.columbia.edu/downloads/data/sdei/sdei-trends-freshwater-availability-grace/sdei-trends-freshwater-availability-grace-2002-2016-geotiff.zip`

        * **Water Mask Data**: Download and place `gpw-v4-data-quality-indicators-rev11_watermask_30_min_tif.zip` in the `./data/` folder. URL: `https://sedac.ciesin.columbia.edu/downloads/data/gpw-v4/gpw-v4-data-quality-indicators-rev11/gpw-v4-data-quality-indicators-rev11_watermask_30_min_tif.zip`

#### **Reproduction Instructions**

To reproduce the quantitative results from the original manuscript, you will need access to the original proprietary input data. Once the data is in the `./data/` folder, the pipeline can be run as described above to replicate the analysis and figures.

-----


### **4. Run the Pipeline**

The demo runs the full analysis pipeline on a provided dataset.

  * **Required Data**: The `00-prepare-data.R` script requires a proprietary Excel file named `copper_data_20250227.xlsx` as described above. Since this file is not provided due to copyright restrictions, you will need to replace the data source to run the full pipeline. The script also downloads several public geospatial datasets.

  * **Instructions**:

    1.  Ensure all dependencies are installed.
    2.  Place your data file, formatted similarly to the original, in the `./data/` directory.
    3.  Download other required geospatial data
    4.  Run the R scripts in the following order:
          * `00-prepare-data.R`
          * `01-train-models.R`
          * `02-predict-water-use.R`
          * `03-water-use-analysis.R`

  * **Expected Output**:
    The scripts will generate a series of `.csv` and `.png` files in the `./results/` directory, including:

      * `rw_maps_*.png`: Maps of raw water use.
      * `tw_maps_*.png`: Maps of total water use.
      * `final_predictions.csv`: The main output file containing predicted water use values.
      * `copper_mine_site_level_water_use_2015-2019.csv`: A formatted table of water use predictions and reference values.
      * `copper_mine_site_level_new_water_slope_2015-2019.csv`: A summary of water use trends and a quadrant analysis.

* **Expected Run Time**:
    The total run time depends heavily on the size of the input data and the speed of your machine. The model training step (01-train-models.R), which involves repeated cross-validation, is the most computationally intensive part. On a normal desktop computer, the full pipeline could take anywhere from 1 to 2 hours.





