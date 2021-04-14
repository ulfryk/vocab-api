const cardsPath = '/cards';
const cardInp = (aSide, aSideDetails, bSide) => ({aSide, aSideDetails, bSide});
const cardPatch = (aSide, aSideDetails, bSide, archived, suspended) => ({
    aSide,
    aSideDetails,
    bSide,
    archived,
    suspended
});

const logResponse = resp => resp.json().then(body => {
    console.log(body);
    return body;
});

const logError = err => {
    console.error(err);
    throw err;
};

const getAll = () => fetch(cardsPath)
    .then(logResponse)
    .catch(logError);

const create = input => fetch(cardsPath, {method: 'POST', body: JSON.stringify(input)})
    .then(logResponse)
    .catch(logError);

const update = (id, patch) => fetch(`${cardsPath}/${id}`, {method: 'PATCH', body: JSON.stringify(patch)})
    .then(logResponse)
    .catch(logError);
