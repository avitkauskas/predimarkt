CREATE TABLE close_market_jobs (
    id UUID DEFAULT uuidv7() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL,
    status job_status DEFAULT 'job_status_not_started' NOT NULL,
    last_error TEXT DEFAULT null,
    attempts_count INT DEFAULT 0 NOT NULL,
    locked_at TIMESTAMP WITH TIME ZONE DEFAULT null,
    locked_by UUID DEFAULT null,
    run_at TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL
);
