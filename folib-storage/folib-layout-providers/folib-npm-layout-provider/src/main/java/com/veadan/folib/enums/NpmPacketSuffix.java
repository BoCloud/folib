package com.veadan.folib.enums;

import lombok.Data;


public enum NpmPacketSuffix {

    TGZ("tgz"),

    JSON("json"),
    HAR("har");
   private String value;

   NpmPacketSuffix(String value){
       this.value = value;
   }
   public String getValue() {
       return value;
   }


}
