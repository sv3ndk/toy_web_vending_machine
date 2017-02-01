package model

import play.api.libs.json.{ JsValue, Json }

object Web {

  def errorResponse(ex: Throwable): JsValue = errorResponse(ex.getMessage)

  def errorResponse(details: String): JsValue = errorJsonResponse(s""" "$details" """)

  /**
   * Builds a JSON error response with the provided details, which must be valid json element
   */
  def errorJsonResponse(jsonDetails: String): JsValue =
    Json.parse(s"""{"error": $jsonDetails}""")

}