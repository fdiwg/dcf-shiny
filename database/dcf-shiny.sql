--
-- PostgreSQL database dump
--

-- Dumped from database version 11.7 (Ubuntu 11.7-1.pgdg18.04+1)
-- Dumped by pg_dump version 15.2

-- Started on 2023-04-19 09:34:19

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- TOC entry 7 (class 2615 OID 2200)
-- Name: public; Type: SCHEMA; Schema: -; Owner: postgres
--

-- *not* creating schema, since initdb creates it


ALTER SCHEMA public OWNER TO postgres;

--
-- TOC entry 2 (class 3079 OID 5644617)
-- Name: postgis; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS postgis WITH SCHEMA public;


--
-- TOC entry 4429 (class 0 OID 0)
-- Dependencies: 2
-- Name: EXTENSION postgis; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION postgis IS 'PostGIS geometry, geography, and raster spatial types and functions';


SET default_tablespace = '';

--
-- TOC entry 197 (class 1259 OID 5642975)
-- Name: dcf_data_call; Type: TABLE; Schema: public; Owner: dcf_shiny_u
--

CREATE TABLE public.dcf_data_call (
    id_data_call integer NOT NULL,
    task_id character varying NOT NULL,
    date_start date NOT NULL,
    date_end date NOT NULL,
    status character varying NOT NULL,
    creator_id character varying NOT NULL,
    creation_date timestamp with time zone NOT NULL,
    updater_id character varying,
    update_date timestamp with time zone
);


ALTER TABLE public.dcf_data_call OWNER TO dcf_shiny_u;

--
-- TOC entry 198 (class 1259 OID 5642981)
-- Name: dcf_users; Type: TABLE; Schema: public; Owner: dcf_shiny_u
--

CREATE TABLE public.dcf_users (
    id_user integer NOT NULL,
    username character varying NOT NULL,
    fullname character varying NOT NULL,
    reporting_entities character varying,
    creator_id character varying NOT NULL,
    creation_date timestamp with time zone NOT NULL,
    updater_id character varying,
    update_date timestamp with time zone,
    roles character varying
);


ALTER TABLE public.dcf_users OWNER TO dcf_shiny_u;

--
-- TOC entry 4290 (class 2606 OID 5642988)
-- Name: dcf_data_call dcf_data_call_pkey; Type: CONSTRAINT; Schema: public; Owner: dcf_shiny_u
--

ALTER TABLE ONLY public.dcf_data_call
    ADD CONSTRAINT dcf_data_call_pkey PRIMARY KEY (task_id, date_start, date_end);


--
-- TOC entry 4292 (class 2606 OID 5642990)
-- Name: dcf_users dcf_users_pkey; Type: CONSTRAINT; Schema: public; Owner: dcf_shiny_u
--

ALTER TABLE ONLY public.dcf_users
    ADD CONSTRAINT dcf_users_pkey PRIMARY KEY (id_user);


--
-- TOC entry 4428 (class 0 OID 0)
-- Dependencies: 7
-- Name: SCHEMA public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE USAGE ON SCHEMA public FROM PUBLIC;
GRANT ALL ON SCHEMA public TO PUBLIC;


-- Completed on 2023-04-19 09:34:24

--
-- PostgreSQL database dump complete
--

