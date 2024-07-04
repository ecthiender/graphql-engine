use axum::{http::StatusCode, Json};
use ndc_models;
use std::collections::BTreeMap;
use uuid::Uuid;

use crate::{
    query::Result,
    types::login::{LoginResponse, ResponseHeaders},
};

pub(crate) fn procedure_info() -> ndc_models::ProcedureInfo {
    ndc_models::ProcedureInfo {
        name: "login".into(),
        description: Some("Perform a user login".into()),
        arguments: BTreeMap::from_iter([
            (
                "_headers".into(),
                ndc_models::ArgumentInfo {
                    description: Some("headers required for authentication".into()),
                    argument_type: ndc_models::Type::Named {
                        name: "HeaderMap".into(),
                    },
                },
            ),
            (
                "username".into(),
                ndc_models::ArgumentInfo {
                    description: Some("username of the user".into()),
                    argument_type: ndc_models::Type::Named {
                        name: "String".into(),
                    },
                },
            ),
            (
                "password".into(),
                ndc_models::ArgumentInfo {
                    description: Some("password of the user".into()),
                    argument_type: ndc_models::Type::Named {
                        name: "String".into(),
                    },
                },
            ),
        ]),
        result_type: ndc_models::Type::Named {
            name: "login_response".into(),
        },
    }
}

pub(crate) fn execute(
    arguments: &BTreeMap<ndc_models::ArgumentName, serde_json::Value>,
) -> Result<serde_json::Value> {
    let _headers = arguments.get("_headers").ok_or((
        StatusCode::BAD_REQUEST,
        Json(ndc_models::ErrorResponse {
            message: "required argument field '_headers' is missing".into(),
            details: serde_json::Value::Null,
        }),
    ))?;
    let _username = arguments.get("username").ok_or((
        StatusCode::BAD_REQUEST,
        Json(ndc_models::ErrorResponse {
            message: "required argument field 'username' is missing".into(),
            details: serde_json::Value::Null,
        }),
    ))?;
    let _password = arguments.get("password").ok_or((
        StatusCode::BAD_REQUEST,
        Json(ndc_models::ErrorResponse {
            message: "required argument field 'password' is missing".into(),
            details: serde_json::Value::Null,
        }),
    ))?;

    let cookie_val1 = Uuid::new_v4();
    let cookie_val2 = Uuid::new_v4();
    let login_response = LoginResponse {
        response: true,
        headers: ResponseHeaders {
            cookie: format!("foo={cookie_val1:}; bar={cookie_val2:};"),
            session_token: Uuid::new_v4().to_string(),
        },
    };
    serde_json::to_value(login_response).map_err(|_| {
        (
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(ndc_models::ErrorResponse {
                message: "cannot encode response".into(),
                details: serde_json::Value::Null,
            }),
        )
    })
}
