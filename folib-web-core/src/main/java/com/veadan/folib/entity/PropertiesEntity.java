package com.veadan.folib.entity;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.Table;
import java.io.Serializable;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Table(name = "properties")
public class PropertiesEntity implements Serializable,Cloneable{

    private String id ;
    private String value ;
}
