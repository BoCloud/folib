package com.veadan.folib.controllers.federal.req;

import lombok.*;
import lombok.experimental.Accessors;

@EqualsAndHashCode(callSuper = true)
@Data
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
public class FederalRepositoryUpdateReq extends FederalRepositoryBaseReq{

    /**
     * id
     */
    private long id;
    /**
     * 策略ID
     */
    private long policyId;
}
