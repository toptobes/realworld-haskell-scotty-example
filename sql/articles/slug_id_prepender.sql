-- yeah so I wanted to append the primary key of the article to the end of the slug for easier and more
-- efficient querying but didn't want to deal with UUIDs because who likes those in their url
-- and wasn't sure how else to do it. 

-- Meh, it works.

-- Ideally, I would do something like `.../:id/:slug` like stackexchaange does (as an example)
-- but I don't think that conforms with the realworld spec

CREATE OR REPLACE FUNCTION prepend_slug_with_id() RETURNS TRIGGER AS $$ 
BEGIN 
  IF TG_OP = 'INSERT' OR (TG_OP = 'UPDATE' AND OLD.slug IS DISTINCT FROM NEW.slug) THEN 
    NEW.slug := NEW.id || '-' || NEW.slug;
  END IF;

  RETURN NEW;
END; 
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS update_slug_with_id ON article;

CREATE TRIGGER update_slug_with_id
  BEFORE INSERT OR UPDATE
  ON article
  FOR EACH ROW EXECUTE FUNCTION prepend_slug_with_id();
