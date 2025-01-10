CREATE DATABASE ExperimentalAnalysisDB;
USE ExperimentalAnalysisDB;


CREATE TABLE PhenotypeProcedures (
    impcParameterOrigId VARCHAR(255) ,
    procedureId VARCHAR(255) NOT NULL
);

CREATE TABLE Parameters (
    impcParameterOrigId INT NOT NULL,
    name VARCHAR(255) NOT NULL,
    description TEXT,
	parameterId VARCHAR(255) PRIMARY KEY
);

CREATE TABLE RawData (
    gene_accession_id VARCHAR(255),
    gene_symbol VARCHAR(255),
    mouse_strain VARCHAR(255),
    mouse_life_stage VARCHAR(255),
    parameter_id VARCHAR(255),
    pvalue FLOAT,
    parameter_name VARCHAR(255),
    analysis_id VARCHAR(255)  
);


CREATE TABLE disease (
    disease_id VARCHAR(255),
    gene_accession_id VARCHAR(255) ,
    phenodigm_score FLOAT
);

CREATE TABLE ParameterGroup (
    group_id INT AUTO_INCREMENT PRIMARY KEY, 
    parameterId VARCHAR(255) NOT NULL,       
    group_name VARCHAR(255),                 
    FOREIGN KEY (parameterId) REFERENCES Parameters(parameterId)
);

INSERT INTO ParameterGroup (parameterId, group_name)
SELECT 
    parameterId, 
    CASE 
        WHEN name LIKE '%brain%' THEN 'Brain Group'
        WHEN name LIKE '%weight%' THEN 'Weight Group'
        WHEN name LIKE '%image%' THEN 'Image Group'
        WHEN name LIKE '%blood%' THEN 'Blood Group'
        WHEN name LIKE '%liver%' THEN 'Liver Group'
        WHEN name LIKE '%skin%' THEN 'Skin Group'
        ELSE 'Other Group'
    END AS group_name
FROM Parameters
WHERE name LIKE '%brain%'
   OR name LIKE '%weight%'
   OR name LIKE '%image%'
   OR name LIKE '%blood%'
   OR name LIKE '%liver%'
   OR name LIKE '%skin%';
   
   ALTER TABLE rawdata
ADD CONSTRAINT pk_rawdata PRIMARY KEY (analysis_id);

 ALTER TABLE rawdata
ADD CONSTRAINT fk_RawData FOREIGN KEY (parameter_id) REFERENCES Parameters(parameterId);

ALTER TABLE phenotypeprocedures 
 ADD CONSTRAINT pk_phenotypeprocedures PRIMARY KEY (impcParameterOrigId);
 
 ALTER TABLE Parameters
MODIFY impcParameterOrigId VARCHAR(255);

 ALTER TABLE parameters
 ADD CONSTRAINT fk_parameters FOREIGN KEY (impcParameterOrigId) REFERENCES phenotypeprocedures(impcParameterOrigId);


SELECT *
FROM rawdata
WHERE gene_symbol LIKE '%Golt1a%'
   OR gene_symbol LIKE '%Chrnd%'
   OR gene_symbol LIKE '%Gpsm1%'
   OR gene_symbol LIKE '%Hook1%';
