module Web.View.Markets.Show where
import Application.Domain.LMSR
import Application.Domain.Types
import qualified Data.List as List
import qualified Data.Map.Strict as M
import qualified Data.Text as Text
import Text.Printf (printf)
import Web.Types (AssetChartData (..), OhlcPoint (..))
import Web.View.Prelude

data ShowView = ShowView
    { market         :: Include' ["assets", "categoryId"] Market
    , tradingAssetId :: Maybe (Id Asset)
    , tradingAction  :: Maybe Text
    , chartData      :: [AssetChartData]
    }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <div class="container-fluid py-3">
            <div class="row g-4">
                <div class="col-12 col-lg-8 order-1 order-lg-1">
                    <div class="card shadow-sm">
                        <div class={classes ["card-header text-muted d-flex justify-content-between align-items-center py-2", (headerClass, True)]}>
                            <span class="ms-2">{market.categoryId.name}</span>
                            <div class="me-2">
                                {statusBadge}
                            </div>
                        </div>
                        <div class="card-body p-4">
                            <header class="mb-4">
                                <div class="d-flex align-items-center gap-2">
                                    <button class="btn btn-outline-secondary back-button flex-shrink-0"
                                            onclick="history.back()"
                                            type="button"
                                            title="Go back">
                                        ←
                                    </button>
                                    <div class="flex-grow-1 overflow-x-auto scroll-no-bar ms-1"
                                         style="white-space: nowrap;">
                                        <span class="h4 fw-bold">
                                            {market.title}
                                        </span>
                                    </div>
                                </div>
                            </header>

                            <div class="assets-list border-top mt-4">
                                {forEach market.assets renderAsset}
                            </div>

                            <div class="mt-4 pt-4">
                                <h6 class="info-label mb-3" style="cursor: pointer;"
                                    onclick="document.getElementById('price-chart').classList.toggle('d-none'); initPriceChart();">
                                    Price Chart
                                </h6>
                                <div id="price-chart" style="height: 300px; margin-bottom: 1.5rem;">
                                </div>
                                {chartDataScript}
                                <!-- Debug: Chart data count: {show (length chartData)} assets -->

                                <h6 class="info-label mb-3" style="cursor: pointer;"
                                    onclick="document.getElementById('market-description').classList.toggle('d-none')">
                                    Rules & Description
                                </h6>
                                <div id="market-description" class="d-none">
                                    <p class="text-muted mb-0">{market.description}</p>
                                </div>
                            </div>
                            {chartScript}
                        </div>
                    </div>
                </div>

                <div class="col-12 col-lg-4 order-2 order-lg-2">
                    <div class="card shadow-sm h-100">
                        <div class="card-header py-2">
                            <span class="fw-semibold">Users Chat</span>
                        </div>
                        <div class="card-body p-3 d-flex flex-column" style="min-height: 300px;">
                            <div class="input-group mt-2">
                                <input type="text" class="form-control" placeholder="Type a message..." />
                                <button class="btn btn-primary" type="button">Send</button>
                            </div>
                            <div class="flex-grow-1 rounded p-3 mb-3" style="overflow-y: auto;">
                                <p class="text-muted">Chat messages will appear here</p>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    |]
        where
            headerClass = marketStatusHeaderClasses market.status
            statusBadge =
                when (market.status /= MarketStatusOpen)
                    [hsx|<span>{marketStatusLabel market.status}</span>|]

            lmsrState = let qtyMap = M.fromList [(a.id, Quantity a.quantity) | a <- market.assets]
                         in (qtyMap, Beta market.beta)

            renderAsset :: Asset -> Html
            renderAsset asset = [hsx|
                <div class={classes ["py-3 border-bottom",
                                     ("market-status-resolved-asset", isResolvedWinner)]}
                     id={"asset-" <> show asset.id}>
                    <div class="d-flex justify-content-between align-items-center">
                        <div class="asset-info ms-3">
                            <div class="fw-semibold fs-5">
                                {asset.name}
                            </div>
                        </div>
                        <div class="asset-actions d-flex align-items-center gap-2">
                            {priceDisplay}
                            {when (isTradable) buySellButtons}
                        </div>
                    </div>

                    {when (isTradable) buySellForms}
                </div>
            |]
                where
                    isTradable = market.status == MarketStatusOpen
                    isResolvedWinner = market.status == MarketStatusResolved
                        && market.outcomeAssetId == Just asset.id
                    isBuyFormOpen = tradingAssetId == Just asset.id && tradingAction == Just "buy"
                    isSellFormOpen = tradingAssetId == Just asset.id && tradingAction == Just "sell"

                    assetPriceVal :: Double
                    assetPriceVal = case market.status of
                        MarketStatusResolved -> if isResolvedWinner then 1.0 else 0.0
                        MarketStatusRefunded -> 0.0
                        _ -> assetPrice asset.id (snd lmsrState) (fst lmsrState)

                    priceDisplay = [hsx|
                        <div class="text-end fw-medium pe-3" style="width: 100px;">
                            €{printf "%.4f" assetPriceVal :: String}
                        </div>
                    |]

                    buySellButtons = [hsx|
                        <div class="d-flex gap-1" style="width: 140px">
                            <button class="btn btn-outline-success btn-sm fw-medium"
                                    type="button"
                                    style="width: calc(50% - 2px);"
                                    onclick={toggleForm (show asset.id) "buy"}>
                                BUY
                            </button>
                            <button class="btn btn-outline-danger btn-sm fw-medium"
                                    type="button"
                                    style="width: calc(50% - 2px);"
                                    onclick={toggleForm (show asset.id) "sell"}>
                                SELL
                            </button>
                        </div>
                    |]

                    buySellForms = [hsx|
                        <div id={"buy-form-" <> show asset.id}
                             class={classes ["mt-3", ("d-none", not isBuyFormOpen)]}>
                            <form action={ExecuteTradeAction asset.id} method="POST">
                                <input type="hidden" name="type" value="buy" />
                                <div class="d-flex flex-column align-items-end gap-2">
                                    <div class="d-flex align-items-center gap-3">
                                        <div class="input-group" style="width: 160px">
                                            <span class="input-group-text info-label">shares</span>
                                            <input type="number" name="quantity"
                                                   value="10" step="1" min="0"
                                                   class="form-control"
                                                   autofocus={isBuyFormOpen}
                                                   oninput="updateBuyInfo(this)"
                                                   data-info-id={"buy-info-" <> show asset.id}
                                                   data-a={show assetPriceVal}
                                                   data-beta={show market.beta} />
                                        </div>
                                        <button type="submit" class="btn btn-primary fw-bold"
                                                style="width: 140px">BUY</button>
                                    </div>
                                    <div id={"buy-info-" <> show asset.id}
                                         class="trade-info-container text-end w-100 d-none"
                                         style="max-width: 320px;">
                                    </div>
                                </div>
                            </form>
                        </div>

                        <div id={"sell-form-" <> show asset.id}
                             class={classes ["mt-3", ("d-none", not isSellFormOpen)]}>
                            <form action={ExecuteTradeAction asset.id} method="POST">
                                <input type="hidden" name="type" value="sell" />
                                <div class="d-flex flex-column align-items-end gap-2">
                                    <div class="d-flex align-items-center gap-3">
                                        <div class="input-group" style="width: 160px">
                                            <span class="input-group-text info-label">shares</span>
                                            <input type="number" name="quantity"
                                                   value="10" step="1" min="0"
                                                   class="form-control"
                                                   autofocus={isSellFormOpen}
                                                   oninput="updateSellInfo(this)"
                                                   data-info-id={"sell-info-" <> show asset.id}
                                                   data-a={show assetPriceVal}
                                                   data-beta={show market.beta} />
                                        </div>
                                        <button type="submit" class="btn btn-primary fw-bold"
                                                style="width: 140px">SELL</button>
                                    </div>
                                    <div id={"sell-info-" <> show asset.id}
                                         class="trade-info-container text-end w-100 d-none"
                                         style="max-width: 320px;">
                                    </div>
                                </div>
                            </form>
                        </div>
                    |]

                    toggleForm :: Text -> Text -> Text
                    toggleForm id type' = "window.toggleAssetForm('" <> id <> "', '" <> type' <> "')"

            -- | Encode chart data as JSON for JavaScript
            encodeChartData :: [AssetChartData] -> Text
            encodeChartData assetsData =
                "[" <> Text.intercalate "," (map encodeAsset assetsData) <> "]"
                where
                    encodeAsset (AssetChartData aid' aname' acolor' ohlcs') =
                        "{\"id\":\"" <> assetIdToText aid' <> "\",\"name\":\"" <> aname' <> "\",\"color\":\"" <> acolor' <> "\",\"data\":[" <> Text.intercalate "," (map encodeOhlc ohlcs') <> "]}"
                    encodeOhlc (OhlcPoint time' open' high' low' close') =
                        "{\"time\":" <> tshow time' <> ",\"open\":" <> tshow open' <> ",\"high\":" <> tshow high' <> ",\"low\":" <> tshow low' <> ",\"close\":" <> tshow close' <> "}"
                    tshow :: Show a => a -> Text
                    tshow x = cs (show x)
                    assetIdToText :: Id Asset -> Text
                    assetIdToText aid = cs (show aid)

            -- | Script tag with chart data (raw HTML to prevent escaping)
            chartDataScript :: Html
            chartDataScript = preEscapedToHtml $ Text.concat
                [ "<script id=\"chart-data\" type=\"application/json\">"
                , encodeChartData chartData
                , "</script>"
                ]

            chartScript :: Html
            chartScript = [hsx|
                <script>
                    function initPriceChart() {
                        var chartContainer = document.getElementById('price-chart');
                        var chartDataScript = document.getElementById('chart-data');
                        if (!chartContainer || !chartDataScript) return;

                        if (chartContainer._chart) {
                            chartContainer._chart.remove();
                            chartContainer._chart = null;
                        }

                        var chartData;
                        try {
                            chartData = JSON.parse(chartDataScript.textContent.trim());
                        } catch (e) {
                            return;
                        }

                        if (!chartData || chartData.length === 0) return;

                        var chart = LightweightCharts.createChart(chartContainer, {
                            width: chartContainer.clientWidth,
                            height: chartContainer.clientHeight,
                            layout: {
                                background: { color: 'transparent' },
                                textColor: getComputedStyle(document.body).getPropertyValue('--bs-body-color') || '#333',
                            },
                            grid: {
                                vertLines: { color: 'rgba(128, 128, 128, 0.2)' },
                                horzLines: { color: 'rgba(128, 128, 128, 0.2)' },
                            },
                            crosshair: {
                                mode: LightweightCharts.CrosshairMode.Normal,
                            },
                            rightPriceScale: {
                                borderColor: 'rgba(128, 128, 128, 0.2)',
                            },
                            timeScale: {
                                borderColor: 'rgba(128, 128, 128, 0.2)',
                                timeVisible: false,
                            },
                        });

                        chartContainer._chart = chart;

                        chartData.forEach(function(asset) {
                            var series = chart.addSeries(LightweightCharts.CandlestickSeries, {
                                upColor: asset.color,
                                downColor: '#ef5350',
                                borderUpColor: asset.color,
                                borderDownColor: '#ef5350',
                                wickUpColor: asset.color,
                                wickDownColor: '#ef5350',
                                title: asset.name,
                            });

                            if (asset.data && asset.data.length > 0) {
                                series.setData(asset.data.sort(function(a, b) { return a.time - b.time; }));
                            }
                        });

                        chart.timeScale().fitContent();

                        var resizeObserver = new ResizeObserver(function(entries) {
                            chart.applyOptions({
                                width: entries[0].contentRect.width,
                                height: entries[0].contentRect.height,
                            });
                        });
                        resizeObserver.observe(chartContainer);
                    }

                    document.addEventListener('turbolinks:load', initPriceChart);

                    document.addEventListener('turbolinks:before-cache', function() {
                        var chartContainer = document.getElementById('price-chart');
                        if (chartContainer && chartContainer._chart) {
                            chartContainer._chart.remove();
                            chartContainer._chart = null;
                        }
                    });
                </script>
            |]
