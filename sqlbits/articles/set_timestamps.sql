-- https://stackoverflow.com/questions/35231697/how-to-let-default-values-come-from-the-database

-- I could've done this in the application code but I agree that it's prob better for the
-- database to be the source of truth on this one. Lmk if you disagree!

CREATE OR REPLACE FUNCTION set_timestamps() RETURNS TRIGGER AS $$
BEGIN
    IF TG_OP = 'INSERT' THEN
        NEW.created := now();
    END IF;
    NEW.updated := now();
    RETURN NEW;   
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS set_article_timestamps ON article;
DROP TRIGGER IF EXISTS set_comment_timestamps ON comment;

CREATE TRIGGER set_article_timestamps
  BEFORE INSERT OR UPDATE
  ON article
  FOR EACH ROW EXECUTE FUNCTION set_timestamps();

CREATE TRIGGER set_comment_timestamps
  BEFORE INSERT OR UPDATE
  ON comment
  FOR EACH ROW EXECUTE FUNCTION set_timestamps();
