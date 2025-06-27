package com.veadan.folib.entity;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.persistence.Column;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

/**
 * @author veadan
 * @since 2024-08-13 16:49
 */

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
@Table(name = "audit_event")
@ApiModel("审计事件")
public class AuditEvent {

    @Id
    @GeneratedValue(generator = "JDBC", strategy = GenerationType.IDENTITY)
    @ApiModelProperty("id")
    @Column(name = "id")
    private Integer id;

    @Column(name = "module_value")
    private String moduleValue;

    @Column(name = "module_name")
    private String moduleName;

    @Column(name = "event_value")
    private String eventValue;


    @Column(name = "event_name")
    private String eventName;

    @Column(name = "used")
    private Integer used;

}
