--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;
SET default_tablespace = '';
SET default_with_oids = false;

--
-- Name: posts; Type: TABLE; Schema: public; Owner: -; Tablespace:
--

Create TABLE products (
  id SERIAL PRIMARY KEY,
  name character varying NOT NULL,
  shelf_id integer REFERENCES shelfs,
  "timestamp" timestamp with time zone DEFAULT now()
  );

Create TABLE items (
  id SERIAL PRIMARY KEY,
  name character varying NOT NULL,
  "timestamp" timestamp with time zone DEFAULT now()
  );

Create TABLE shelfitems (
  id SERIAL PRIMARY KEY,
  quantity integer,
  shelf_id integer REFERENCES shelfs,
  item_id integer REFERENCES shelfs,
  "timestamp" timestamp with time zone DEFAULT now()
  );

Create TABLE shelfs (
  id SERIAL PRIMARY KEY,
  label character varying NOT NULL,
  position character varying NOT NULL DEFAULT '',
  size integer,
  "timestamp" timestamp with time zone DEFAULT now()
  -- room_id INT
  );


CREATE TABLE posts (
    id integer NOT NULL,
    title character varying NOT NULL,
    body character varying NOT NULL,
    users_email character varying NOT NULL,
    "timestamp" timestamp with time zone DEFAULT now()
);


--
-- Name: posts_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

Create SEQUENCE posts_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: posts_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE posts_id_seq OWNED BY posts.id;


--
-- Name: users; Type: TABLE; Schema: public; Owner: -; Tablespace:
--

CREATE TABLE users (
    email character varying NOT NULL,
    password bytea NOT NULL
);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY posts ALTER COLUMN id SET DEFAULT nextval('posts_id_seq'::regclass);


--
-- Name: posts_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace:
--

ALTER TABLE ONLY posts
    ADD CONSTRAINT posts_pkey PRIMARY KEY (id);


--
-- Name: users_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace:
--

ALTER TABLE ONLY users
    ADD CONSTRAINT users_pkey PRIMARY KEY (email);


--
-- Name: posts_user_fk; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY posts
    ADD CONSTRAINT posts_user_fk FOREIGN KEY (users_email) REFERENCES users(email);


--
-- Name: public; Type: ACL; Schema: -; Owner: -
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM pgsql;
GRANT  ALL ON SCHEMA public TO pgsql;
GRANT  ALL ON SCHEMA public TO PUBLIC;

-- GRANT ALL PRIVILEGES ON products TO blogtutorial;
-- GRANT USAGE, SELECT  ON SEQUENCE products_id_seq TO blogtutorial;

--
-- PostgreSQL database dump complete
--
