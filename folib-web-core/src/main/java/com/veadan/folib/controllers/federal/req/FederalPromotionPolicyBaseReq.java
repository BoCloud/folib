package com.veadan.folib.controllers.federal.req;

import lombok.*;
import lombok.experimental.Accessors;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
public class FederalPromotionPolicyBaseReq {

    /**
     * 联邦晋级策略名
     */
    private String name;
    /**
     * 是否开启策略
     */
    private Boolean isEnabled;
    /**
     * 标签[default:标记为老数据适配，latest:标记新建的]
     */
    private String tag;
}
