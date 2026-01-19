module Web.View.Markets.Show where
import Web.View.Prelude

data ShowView = ShowView { market :: Market }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <h1>Show Market</h1>
        <p>{market}</p>

    |]
        where
            breadcrumb = renderBreadcrumb
                            [ breadcrumbLink "Markets" MarketsAction
                            , breadcrumbText "Show Market"
                            ]
