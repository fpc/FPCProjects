/* Don't forget to create the users first with:                                            */
/*                                                                                         */
/* gsec -user sysdba -password masterkey -add fpcbot -pw botpass -fname fpcbot             */
/* gsec -user sysdba -password masterkey -add cgifpc -pw cgipass -fname cgifpc             */
/*                                                                                         */
/* then:                                                                                   */
/*                                                                                         */
/* ibsql < create_database_script                                                          */

SET SQL DIALECT 3;

SET NAMES NONE;

CREATE DATABASE 'fpcbot.fdb'
USER 'SYSDBA' PASSWORD 'masterkey'
PAGE_SIZE 1024
DEFAULT CHARACTER SET NONE;

/******************************************************************************/
/****                              Generators                              ****/
/******************************************************************************/

CREATE GENERATOR GEN_LOGLINEID;
SET GENERATOR GEN_LOGLINEID TO 1;

CREATE GENERATOR GEN_DEFINITIONID;
SET GENERATOR GEN_DEFINITIONID TO 1;

/******************************************************************************/
/****                                Tables                                ****/
/******************************************************************************/

CREATE TABLE TBL_LOGLINES (
    LOGLINEID  INTEGER NOT NULL,
    SENDER     VARCHAR(50) CHARACTER SET NONE,
    RECIEVER   VARCHAR(50) CHARACTER SET NONE,
    MSG        VARCHAR(4096) CHARACTER SET NONE,
    LOGTIME    TIMESTAMP
);

CREATE TABLE TBL_DEFINITIONS (
    DEFINITIONID  INTEGER NOT NULL,
    DEFINITION    VARCHAR(20) NOT NULL,
    DESCRIPTION   VARCHAR(255)
);

/******************************************************************************/
/****                             Primary Keys                             ****/
/******************************************************************************/

ALTER TABLE TBL_LOGLINES ADD CONSTRAINT PK_TBL_LOGLINES PRIMARY KEY (LOGLINEID);

alter table tbl_Definitions add constraint pk_definitions primary key (definitionid);

alter table tbl_Definitions add constraint unq_definitions unique (definition);

/******************************************************************************/
/****                              Privileges                              ****/
/******************************************************************************/

/* Privileges of users */

GRANT SELECT ON TBL_LOGLINES TO CGIFPC;
GRANT INSERT ON TBL_LOGLINES TO FPCBOT;
GRANT SELECT ON TBL_LOGLINES TO FPCBOT;
GRANT INSERT ON TBL_DEFINITIONS TO FPCBOT;
GRANT SELECT ON TBL_DEFINITIONS TO FPCBOT;
GRANT UPDATE ON TBL_DEFINITIONS TO FPCBOT;