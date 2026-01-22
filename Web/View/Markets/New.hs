module Web.View.Markets.New where
import Web.View.Prelude


instance CanSelect Category where
    type SelectValue Category = Id Category
    selectValue = get #id
    selectLabel = get #name

data NewView = NewView { market :: Market, categories :: [Category] }

instance View NewView where
    html NewView { .. } = [hsx|
        <h1>New Market</h1>
        {renderForm market categories}
    |]

renderForm :: Market -> [Category] -> Html
renderForm market categories = formFor market [hsx|
    {(textField #title)}
    {(textField #description)}
    {(selectField #categoryId categories)}
    {closedAtField}
    {submitButton}
|]
    where
        closedAtField = [hsx|
            <div class="mb-3" id="form-group-market_closedAt">
                <label for="market_closedAt" class="form-label">Will close at</label>
                <input
                    type="text"
                    id="market_closedAt"
                    name="closedAt"
                    value={show (get #closedAt market)}
                    class="form-control js-flatpickr"
                    data-enable-time="true"
                    />
            </div>
        |]
