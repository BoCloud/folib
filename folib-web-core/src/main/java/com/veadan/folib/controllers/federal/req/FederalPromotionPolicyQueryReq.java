package com.veadan.folib.controllers.federal.req;

import lombok.*;
import lombok.experimental.Accessors;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;

@Setter
@Getter
@Data
@Accessors(chain = true)
public class FederalPromotionPolicyQueryReq {

    private String name;
    private Boolean isEnabled;
    private String tag;
    private Integer pageNumber;
    private Integer pageSize;

}
