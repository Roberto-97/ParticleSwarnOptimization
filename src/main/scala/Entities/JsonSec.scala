package Entities

import com.fasterxml.jackson.annotation.JsonProperty


case class data(value: Double,time: Double)

case class JsonSec( @JsonProperty name: String, @JsonProperty series: Seq[data])