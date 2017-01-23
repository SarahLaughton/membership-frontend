// ----- Imports ----- //

import listeners from 'src/modules/form/payment/listeners';
import * as payment from 'src/modules/payment';


// ----- Functions ----- //

// Handles the paypal setup response and retrieves the token.
function handleSetupResponse (response) {

	if (response.status === 200) {
		return response.json();
	} else {
		throw new Error('PayPal payment setup request failed.');
	}

}

// Checks that the PayPal token is ok.
function handleTokenRetrieval ({token}) {

	if (token) {
		resolve(token);						
	} else {
		throw new Error('PayPal token came back blank.');
	}

}

// Sends request to server to setup payment, and returns Paypal token.
function setupPayment (resolve, reject) {

	payment.clearErrors();

	if (payment.validateForm()) {

		const SETUP_PAYMENT_URL = '/paypal/setup-payment';

		fetch(SETUP_PAYMENT_URL, { method: 'POST' })
			.then(handleSetupResponse)
			.then(handleTokenRetrieval)
			.catch(err => {

				payment.error(err.message);
				reject(err);

			});

	} else {
		reject('Form invalid.');
	}

}

// Attempts to take the payment and make the user a member.
function makePayment (data, actions) {

	payment.showSpinner();

	createAgreement(data).then(baid => {

		if (baid) {
			postForm(baid);
		}

	}).catch(payment.fail);

}

// Handles the retrieval of the BAID from and AJAX response.
function handleAgreementResponse (response) {

	if (response.status === 200) {
		return response.json();
	} else {

		handleAgreementFail('Agreement request failed.');
		return null;

	}

}

// Handles failure to create agreement.
function handleAgreementFail (err) {

	payment.fail({
		type: 'PayPal',
		code: 'BAIDCreationFailed',
		additional: err
	});

	return null;

}

// Creates the billing agreement and retrieves the BAID as json.
function createAgreement (paypalData) {

	const CREATE_AGREEMENT_URL = '/paypal/create-agreement';

	return fetch(CREATE_AGREEMENT_URL, {

		headers: { 'Content-Type': 'application/json' },
		method: 'POST',
		body: JSON.stringify({ token: paypalData.paymentToken })

	}).then(handleAgreementResponse).catch(handleAgreementFail);

}

// Creates the new member by posting the form data with the BAID.
function postForm (baid) {
	payment.postForm({ 'payment.payPalBaid': baid.token });
}


// ----- Exports ----- //

export function init () {

	paypal.Button.render({

		// Sets the environment.
		env: 'sandbox',
		// Styles the button.
		style: { color: 'blue', size: 'medium' },
		// Defines whether user sees 'continue' or 'pay now' in overlay.
		commit: true,

		// Called when user clicks Paypal button.
		payment: setupPayment,
		// Called when there is an error with the paypal payment.
        onError: () => {
        	payment.fail({ type: 'PayPal', code: 'PaymentError' });
        },
        // We don't want to do anything here, but if this callback isn't present
        // PayPal throws a bunch of errors and then stops working.
        onCancel: () => {},

		// Called when user finishes with Paypal interface (approves payment).
		onAuthorize: makePayment

	}, '#paypal-button-checkout');

	listeners.cardDisplayButtonListener();

}
