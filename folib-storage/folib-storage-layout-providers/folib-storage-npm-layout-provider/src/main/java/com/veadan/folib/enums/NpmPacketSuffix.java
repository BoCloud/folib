package com.veadan.folib.enums;


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
