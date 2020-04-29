package Entities

import com.fasterxml.jackson.annotation.JsonProperty


case class data(name:String,value: Double)

case class JsonSec( @JsonProperty name: String, @JsonProperty series: Seq[data])