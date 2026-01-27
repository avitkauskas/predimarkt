module Web.View.Markets.Show where
import Web.View.Prelude

data ShowView = ShowView { market :: Include "assets" Market }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <h1>{market.title}</h1>
        <p>{market.slug}</p>
        <p>{market.description}</p>

        <div class="mt-4">
            <div class="d-flex justify-content-between align-items-center mb-3">
                <h2>Assets</h2>
            </div>
            <table class="table">
                <thead>
                    <tr>
                        <th>Name</th>
                        <th>Label</th>
                        <th>Status</th>
                    </tr>
                </thead>
                <tbody>
                    {forEach market.assets renderAsset}
                </tbody>
            </table>
        </div>
    |]
        where
            breadcrumb = renderBreadcrumb
                            [ breadcrumbLink "Markets" MarketsAction
                            , breadcrumbText "Show Market"
                            ]

renderAsset :: Asset -> Html
renderAsset asset = [hsx|
    <tr>
        <td>{asset.name}</td>
        <td>{asset.symbol}</td>
        <td>{asset.status}</td>
    </tr>
|]
