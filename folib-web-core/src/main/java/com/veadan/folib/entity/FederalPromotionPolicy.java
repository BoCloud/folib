package com.veadan.folib.entity;

import lombok.*;
import lombok.experimental.Accessors;

import javax.persistence.*;
import java.io.Serializable;
import java.util.Date;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
@Table(name = "federal_promotion_policy")
public class FederalPromotionPolicy implements Serializable {
    /**
     * id
     */
    @Id
    @Column(name = "policy_id")
    @GeneratedValue(generator = "JDBC",strategy = GenerationType.IDENTITY)
    private Long policyId;
    /**
     * 联邦晋级策略名
     */
    private String name;
    /**
     * 是否开启策略
     */
    @Column(name = "is_enabled")
    private Boolean isEnabled;
    /**
     * 标签[default:标记为老数据适配，latest:标记新建的]
     */
    private String tag;
    /**
     * 创建时间
     */
    @Column(name = "create_time")
    private Date createdTime;
    /**
     * 更新时间
     */
    @Column(name = "update_time")
    private Date updateTime;
    /**
     * 创建人
     */
    @Column(name = "create_by")
    private String createdBy;
    /**
     * 更新人
     */
    @Column(name = "update_by")
    private String updatedBy;

    /**
     * 是否同步删除
     */
    @Column(name = "is_delete_sync")
    private Boolean isDeleteSync;

    public void setPolicyId(Long policyId) {
        this.policyId = policyId;
    }

    public Long getPolicyId() {
        return policyId;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public void setIsEnabled(Boolean enabled) {
        this.isEnabled = enabled;
    }

    public Boolean getIsEnabled() {
        return isEnabled;
    }

    public void setTag(String tag) {
        this.tag = tag;
    }

    public String getTag() {
        return tag;
    }

    public void setCreatedTime(Date createdTime) {
        this.createdTime = createdTime;
    }

    public Date getCreatedTime() {
        return createdTime;
    }

    public void setUpdateTime(Date updateTime) {
        this.updateTime = updateTime;
    }

    public Date getUpdateTime() {
        return updateTime;
    }

    public void setCreatedBy(String createdBy) {
        this.createdBy = createdBy;
    }

    public String getCreatedBy() {
        return createdBy;
    }

    public void setUpdatedBy(String updatedBy) {
        this.updatedBy = updatedBy;
    }

    public String getUpdatedBy() {
        return updatedBy;
    }

    public void setIsDeleteSync(Boolean isDeleteSync) {
        this.isDeleteSync = isDeleteSync;
    }

    public Boolean getIsDeleteSync() {
        return isDeleteSync;
    }


}
