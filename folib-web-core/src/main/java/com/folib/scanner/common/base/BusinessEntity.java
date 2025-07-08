

package com.folib.scanner.common.base;

import lombok.Data;

import java.util.Date;

/**
 *
 * @author Veadan
 * @version 2018/1/13.
 */
@Data
public class BusinessEntity extends BaseEntity {
    private String crtUserId;
    private String crtUserName;
    private Date crtTime;

    private String updUserId;
    private String updUserName;
    private Date updTime;
}
