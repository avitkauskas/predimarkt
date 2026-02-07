
\restrict 7NWi2oedOiYkJtXwDVu7bdKf38MmfMGl9a5JyebXgvdnW3tl84Aae9MWWu5xn3v


SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET transaction_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;


SET SESSION AUTHORIZATION DEFAULT;

ALTER TABLE public.admins DISABLE TRIGGER ALL;



ALTER TABLE public.admins ENABLE TRIGGER ALL;


ALTER TABLE public.categories DISABLE TRIGGER ALL;

INSERT INTO public.categories (id, name, slug, sort_idx) VALUES ('bc0c30dc-e041-4334-8410-0c968978e60a', 'Politics', 'politics', 10);
INSERT INTO public.categories (id, name, slug, sort_idx) VALUES ('2c9fa3b7-41bd-4f50-aef8-2c3d83a0737e', 'Economics', 'economics', 20);
INSERT INTO public.categories (id, name, slug, sort_idx) VALUES ('83dee0c6-6a7b-4af2-8167-46d82261d83b', 'Sports', 'sports', 30);
INSERT INTO public.categories (id, name, slug, sort_idx) VALUES ('a15c3996-1389-4e2d-b4b3-ce2c38327c59', 'Other', 'other', 90);


ALTER TABLE public.categories ENABLE TRIGGER ALL;


ALTER TABLE public.users DISABLE TRIGGER ALL;

INSERT INTO public.users (id, email, nickname, password_hash, locked_at, failed_login_attempts, created_at, updated_at) VALUES ('2169acd2-e43c-4628-82b0-d37556a91a44', 'alvydas@vitkauskas.lt', 'alvydas', 'sha256|17|sU0r9V6Va0fUSQZEpZDdOg==|QnItnEOZHTOWBwsdkaPta1FWfgjwXM05hXD/e0Hb1l4=', NULL, 0, '2026-02-06 22:38:18.976001+00', '2026-02-06 22:38:18.976001+00');
INSERT INTO public.users (id, email, nickname, password_hash, locked_at, failed_login_attempts, created_at, updated_at) VALUES ('a6bffceb-db33-4203-b55b-306d5f8da956', 'alvydas.vitkauskas@gmail.com', 'alvydas.vitkauskas', 'sha256|17|zfgOVb6owNhe1I/A7zXpSg==|0JlQvbSwTqldIOms97U09lWCaRFwFKCxR56xqMtu/Tk=', NULL, 0, '2026-02-06 22:31:52.953718+00', '2026-02-06 22:42:59.898926+00');


ALTER TABLE public.users ENABLE TRIGGER ALL;


ALTER TABLE public.markets DISABLE TRIGGER ALL;

INSERT INTO public.markets (id, user_id, title, slug, description, category_id, beta, status, opened_at, closed_at, resolved_at, refunded_at, created_at, updated_at, trades, volume, turnover) VALUES ('a184b5de-8677-4cb1-9aaf-f0a9dcb56447', 'a6bffceb-db33-4203-b55b-306d5f8da956', 'Will Donald Trump be impeached before the end of his cadence?', 'will-donald-trump-be-impeached-before-the-end-of-his-cadence', 'Impeachment', 'bc0c30dc-e041-4334-8410-0c968978e60a', 300, 'market_status_open', '2026-02-07 09:03:03.285001+00', '2029-02-01 00:00:00+00', NULL, NULL, '2026-02-07 09:02:24.97957+00', '2026-02-07 09:56:41.228079+00', 1, 150, 8428);
INSERT INTO public.markets (id, user_id, title, slug, description, category_id, beta, status, opened_at, closed_at, resolved_at, refunded_at, created_at, updated_at, trades, volume, turnover) VALUES ('f851b14b-956e-4169-97f4-7708513a03db', 'a6bffceb-db33-4203-b55b-306d5f8da956', 'Who will win Formula 1 World Title 2026?', 'who-will-win-formula-1-world-title-2026', 'Formula 1 World Title 2026', '83dee0c6-6a7b-4af2-8167-46d82261d83b', 300, 'market_status_open', '2026-02-06 22:37:05.494209+00', '2026-12-10 00:00:00+00', NULL, NULL, '2026-02-06 22:37:01.829112+00', '2026-02-07 10:20:42.401458+00', 4, 200, 2921);
INSERT INTO public.markets (id, user_id, title, slug, description, category_id, beta, status, opened_at, closed_at, resolved_at, refunded_at, created_at, updated_at, trades, volume, turnover) VALUES ('c409054f-fb9c-4902-83fa-97b9692afca5', 'a6bffceb-db33-4203-b55b-306d5f8da956', 'Will index be more than today?', 'will-index-be-more-than-today', 'Question.', '2c9fa3b7-41bd-4f50-aef8-2c3d83a0737e', 300, 'market_status_open', '2026-02-07 10:22:46.538121+00', '2026-02-21 00:00:00+00', NULL, NULL, '2026-02-07 10:22:44.008562+00', '2026-02-07 10:32:45.845368+00', 2, 150, 7396);
INSERT INTO public.markets (id, user_id, title, slug, description, category_id, beta, status, opened_at, closed_at, resolved_at, refunded_at, created_at, updated_at, trades, volume, turnover) VALUES ('e7591fe5-c7c2-4462-a3f8-72249235507a', 'a6bffceb-db33-4203-b55b-306d5f8da956', 'Will average temperature exceed +2% in the next 10 years?', 'will-average-temperature-exceed-2-in-the-next-10-years', 'Climate', 'a15c3996-1389-4e2d-b4b3-ce2c38327c59', 300, 'market_status_open', '2026-02-07 10:37:30.940401+00', '2030-02-01 00:00:00+00', NULL, NULL, '2026-02-07 10:37:28.244288+00', '2026-02-07 10:38:15.465151+00', 1, 80, 4266);


ALTER TABLE public.markets ENABLE TRIGGER ALL;


ALTER TABLE public.assets DISABLE TRIGGER ALL;

INSERT INTO public.assets (id, market_id, name, symbol, quantity, updated_at) VALUES ('8c5be574-9fef-4b39-b2e6-861e3ce41465', 'f851b14b-956e-4169-97f4-7708513a03db', 'Lewis Hamilton', 'HAM', 0, '2026-02-06 22:37:01.829112+00');
INSERT INTO public.assets (id, market_id, name, symbol, quantity, updated_at) VALUES ('cfe55b5d-52e4-4f46-aff6-68f9b15d140e', 'f851b14b-956e-4169-97f4-7708513a03db', 'Charles Leclerc', 'LEC', 0, '2026-02-06 22:37:01.829112+00');
INSERT INTO public.assets (id, market_id, name, symbol, quantity, updated_at) VALUES ('d9da1d75-8534-4e0a-a8c0-2df8097f4c24', 'f851b14b-956e-4169-97f4-7708513a03db', 'Oscar Piastri', 'PIA', 0, '2026-02-06 22:37:01.829112+00');
INSERT INTO public.assets (id, market_id, name, symbol, quantity, updated_at) VALUES ('2e961af3-ae07-405a-a49c-f8614c953ea6', 'f851b14b-956e-4169-97f4-7708513a03db', 'Kimi Antonelli', 'ANT', 0, '2026-02-06 22:37:01.829112+00');
INSERT INTO public.assets (id, market_id, name, symbol, quantity, updated_at) VALUES ('d7b608ab-882d-40f8-97ca-c37fb9d998ca', 'f851b14b-956e-4169-97f4-7708513a03db', 'Lando Noris', 'NOR', 50, '2026-02-06 23:03:08.042795+00');
INSERT INTO public.assets (id, market_id, name, symbol, quantity, updated_at) VALUES ('566856ab-f5c6-489e-9dc5-f0878c871253', 'f851b14b-956e-4169-97f4-7708513a03db', 'Max Verstappen', 'VER', 100, '2026-02-07 07:55:54.248308+00');
INSERT INTO public.assets (id, market_id, name, symbol, quantity, updated_at) VALUES ('1a707d01-9fac-4b51-bceb-ad7721a1d011', 'f851b14b-956e-4169-97f4-7708513a03db', 'George Russell', 'RUS', 60, '2026-02-07 07:58:27.483132+00');
INSERT INTO public.assets (id, market_id, name, symbol, quantity, updated_at) VALUES ('09b2a023-859c-4d3c-82f9-90f3cad43f1f', 'a184b5de-8677-4cb1-9aaf-f0a9dcb56447', 'No - this will hardly ever happen', 'No', 0, '2026-02-07 09:03:00.982405+00');
INSERT INTO public.assets (id, market_id, name, symbol, quantity, updated_at) VALUES ('14abab7c-70a5-4495-b9a0-4d0563396e3c', 'a184b5de-8677-4cb1-9aaf-f0a9dcb56447', 'Yes - I am absolutely sure about that', 'Yes', 150, '2026-02-07 09:56:41.228079+00');
INSERT INTO public.assets (id, market_id, name, symbol, quantity, updated_at) VALUES ('280cb2f9-d28d-4cbd-ae19-f38afd659ffa', 'c409054f-fb9c-4902-83fa-97b9692afca5', 'No', 'No', 0, '2026-02-07 10:22:44.008562+00');
INSERT INTO public.assets (id, market_id, name, symbol, quantity, updated_at) VALUES ('76ef6646-4a95-4e51-bd87-47536d28f287', 'c409054f-fb9c-4902-83fa-97b9692afca5', 'Yes', 'Yes', 50, '2026-02-07 10:32:45.845368+00');
INSERT INTO public.assets (id, market_id, name, symbol, quantity, updated_at) VALUES ('c8bb789c-c74a-4832-9922-acca6296f5ba', 'e7591fe5-c7c2-4462-a3f8-72249235507a', 'Yes', 'Yes', 0, '2026-02-07 10:37:28.244288+00');
INSERT INTO public.assets (id, market_id, name, symbol, quantity, updated_at) VALUES ('821b854f-cc67-4733-804c-2a657d3eada7', 'e7591fe5-c7c2-4462-a3f8-72249235507a', 'No', 'No', 80, '2026-02-07 10:38:15.465151+00');


ALTER TABLE public.assets ENABLE TRIGGER ALL;


ALTER TABLE public.holdings DISABLE TRIGGER ALL;

INSERT INTO public.holdings (id, user_id, market_id, asset_id, quantity, amount_cents, updated_at) VALUES ('ad65d794-7988-46d0-881e-0267e4ccca2a', '2169acd2-e43c-4628-82b0-d37556a91a44', 'f851b14b-956e-4169-97f4-7708513a03db', '1a707d01-9fac-4b51-bceb-ad7721a1d011', 80, 1215, '2026-02-06 22:38:41.140553+00');
INSERT INTO public.holdings (id, user_id, market_id, asset_id, quantity, amount_cents, updated_at) VALUES ('298bbb0a-0e73-425d-9160-45a62660a360', 'a6bffceb-db33-4203-b55b-306d5f8da956', 'f851b14b-956e-4169-97f4-7708513a03db', 'd7b608ab-882d-40f8-97ca-c37fb9d998ca', 50, 698, '2026-02-06 23:03:08.065146+00');
INSERT INTO public.holdings (id, user_id, market_id, asset_id, quantity, amount_cents, updated_at) VALUES ('64d6e468-b3b1-4543-82bb-20828500f5bf', 'a6bffceb-db33-4203-b55b-306d5f8da956', 'f851b14b-956e-4169-97f4-7708513a03db', '566856ab-f5c6-489e-9dc5-f0878c871253', 100, 1649, '2026-02-07 07:55:54.248308+00');
INSERT INTO public.holdings (id, user_id, market_id, asset_id, quantity, amount_cents, updated_at) VALUES ('123ef09f-75c9-4558-b056-986ef72e81ee', 'a6bffceb-db33-4203-b55b-306d5f8da956', 'f851b14b-956e-4169-97f4-7708513a03db', '1a707d01-9fac-4b51-bceb-ad7721a1d011', -20, -323, '2026-02-07 07:58:27.483132+00');
INSERT INTO public.holdings (id, user_id, market_id, asset_id, quantity, amount_cents, updated_at) VALUES ('4b992c9c-314a-4338-87b0-ea2be23b0480', 'a6bffceb-db33-4203-b55b-306d5f8da956', 'a184b5de-8677-4cb1-9aaf-f0a9dcb56447', '14abab7c-70a5-4495-b9a0-4d0563396e3c', 150, 8428, '2026-02-07 09:56:41.228079+00');
INSERT INTO public.holdings (id, user_id, market_id, asset_id, quantity, amount_cents, updated_at) VALUES ('af1ed9c3-d33c-445d-972b-598301729841', 'a6bffceb-db33-4203-b55b-306d5f8da956', 'c409054f-fb9c-4902-83fa-97b9692afca5', '76ef6646-4a95-4e51-bd87-47536d28f287', 50, 2604, '2026-02-07 10:32:45.845368+00');
INSERT INTO public.holdings (id, user_id, market_id, asset_id, quantity, amount_cents, updated_at) VALUES ('2b71ce27-3e8b-4445-adb1-497f716c2e8a', 'a6bffceb-db33-4203-b55b-306d5f8da956', 'e7591fe5-c7c2-4462-a3f8-72249235507a', '821b854f-cc67-4733-804c-2a657d3eada7', 80, 4266, '2026-02-07 10:38:15.465151+00');


ALTER TABLE public.holdings ENABLE TRIGGER ALL;


ALTER TABLE public.schema_migrations DISABLE TRIGGER ALL;

INSERT INTO public.schema_migrations (revision) VALUES (1770416958);
INSERT INTO public.schema_migrations (revision) VALUES (1770448098);
INSERT INTO public.schema_migrations (revision) VALUES (1770449042);


ALTER TABLE public.schema_migrations ENABLE TRIGGER ALL;


ALTER TABLE public.transactions DISABLE TRIGGER ALL;

INSERT INTO public.transactions (id, user_id, market_id, asset_id, quantity, amount_cents, created_at) VALUES ('25c4d470-ab8f-480f-b872-1614c6723573', 'a6bffceb-db33-4203-b55b-306d5f8da956', 'f851b14b-956e-4169-97f4-7708513a03db', '566856ab-f5c6-489e-9dc5-f0878c871253', 100, 1649, '2026-02-06 22:37:40.711654+00');
INSERT INTO public.transactions (id, user_id, market_id, asset_id, quantity, amount_cents, created_at) VALUES ('6161e379-e595-43b5-add8-90f9e16f699b', '2169acd2-e43c-4628-82b0-d37556a91a44', 'f851b14b-956e-4169-97f4-7708513a03db', '1a707d01-9fac-4b51-bceb-ad7721a1d011', 80, 1215, '2026-02-06 22:38:41.138961+00');
INSERT INTO public.transactions (id, user_id, market_id, asset_id, quantity, amount_cents, created_at) VALUES ('fee79cf0-43f4-4371-8683-2a5792b1f4a5', 'a6bffceb-db33-4203-b55b-306d5f8da956', 'f851b14b-956e-4169-97f4-7708513a03db', 'd7b608ab-882d-40f8-97ca-c37fb9d998ca', 50, 698, '2026-02-06 23:03:08.060554+00');
INSERT INTO public.transactions (id, user_id, market_id, asset_id, quantity, amount_cents, created_at) VALUES ('e272bc01-5664-4ba2-88d5-7cc8be066ebc', 'a6bffceb-db33-4203-b55b-306d5f8da956', 'f851b14b-956e-4169-97f4-7708513a03db', '566856ab-f5c6-489e-9dc5-f0878c871253', 10, 179, '2026-02-07 07:48:47.153405+00');
INSERT INTO public.transactions (id, user_id, market_id, asset_id, quantity, amount_cents, created_at) VALUES ('e49a1e93-26c4-4a00-86f9-01f41b7aefbc', 'a6bffceb-db33-4203-b55b-306d5f8da956', 'f851b14b-956e-4169-97f4-7708513a03db', '566856ab-f5c6-489e-9dc5-f0878c871253', -10, 179, '2026-02-07 07:55:54.248308+00');
INSERT INTO public.transactions (id, user_id, market_id, asset_id, quantity, amount_cents, created_at) VALUES ('6ddbb825-c223-446b-b8bb-b39d6f1e511e', 'a6bffceb-db33-4203-b55b-306d5f8da956', 'f851b14b-956e-4169-97f4-7708513a03db', '1a707d01-9fac-4b51-bceb-ad7721a1d011', -100, 1443, '2026-02-07 07:57:45.795772+00');
INSERT INTO public.transactions (id, user_id, market_id, asset_id, quantity, amount_cents, created_at) VALUES ('8f8befa1-2e72-4a50-b623-9519dd9550a7', 'a6bffceb-db33-4203-b55b-306d5f8da956', 'f851b14b-956e-4169-97f4-7708513a03db', '1a707d01-9fac-4b51-bceb-ad7721a1d011', 80, 1120, '2026-02-07 07:58:27.483132+00');
INSERT INTO public.transactions (id, user_id, market_id, asset_id, quantity, amount_cents, created_at) VALUES ('1c69328a-076c-43b6-a13c-e8eb72c291e9', 'a6bffceb-db33-4203-b55b-306d5f8da956', 'a184b5de-8677-4cb1-9aaf-f0a9dcb56447', '14abab7c-70a5-4495-b9a0-4d0563396e3c', 150, 8428, '2026-02-07 09:56:41.228079+00');
INSERT INTO public.transactions (id, user_id, market_id, asset_id, quantity, amount_cents, created_at) VALUES ('cfcfc58a-9d0f-45a0-97fd-1a879f1d3083', 'a6bffceb-db33-4203-b55b-306d5f8da956', 'c409054f-fb9c-4902-83fa-97b9692afca5', '76ef6646-4a95-4e51-bd87-47536d28f287', -50, 2396, '2026-02-07 10:32:25.778972+00');
INSERT INTO public.transactions (id, user_id, market_id, asset_id, quantity, amount_cents, created_at) VALUES ('a1cce680-cd92-46f8-95f3-91559e249e43', 'a6bffceb-db33-4203-b55b-306d5f8da956', 'c409054f-fb9c-4902-83fa-97b9692afca5', '76ef6646-4a95-4e51-bd87-47536d28f287', 100, 5000, '2026-02-07 10:32:45.845368+00');
INSERT INTO public.transactions (id, user_id, market_id, asset_id, quantity, amount_cents, created_at) VALUES ('ab2405b8-fc8b-4ed6-b172-d4ee711d2acd', 'a6bffceb-db33-4203-b55b-306d5f8da956', 'e7591fe5-c7c2-4462-a3f8-72249235507a', '821b854f-cc67-4733-804c-2a657d3eada7', 80, 4266, '2026-02-07 10:38:15.465151+00');


ALTER TABLE public.transactions ENABLE TRIGGER ALL;


ALTER TABLE public.wallets DISABLE TRIGGER ALL;

INSERT INTO public.wallets (id, user_id, amount_cents) VALUES ('f9c5a88b-7455-48d4-93b5-0fc0ca83e21e', '2169acd2-e43c-4628-82b0-d37556a91a44', 98785);
INSERT INTO public.wallets (id, user_id, amount_cents) VALUES ('05b01d9a-8cee-4ea2-a2f0-dd60e180a412', 'a6bffceb-db33-4203-b55b-306d5f8da956', 82678);


ALTER TABLE public.wallets ENABLE TRIGGER ALL;


\unrestrict 7NWi2oedOiYkJtXwDVu7bdKf38MmfMGl9a5JyebXgvdnW3tl84Aae9MWWu5xn3v

