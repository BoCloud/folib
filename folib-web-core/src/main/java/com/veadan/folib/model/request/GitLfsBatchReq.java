package com.veadan.folib.model.request;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.List;

@Data
@Accessors(chain = true)
@ApiModel(description = "git lfs batch")
public class GitLfsBatchReq {

    @ApiModelProperty("操作：upload ,download")
    private String operation;

    @ApiModelProperty("对象的数组")
    private List<LfsObjectReq> objects;

    @ApiModelProperty("客户端已配置的传输适配器的可选字符串标识符数组")
    private List<String> transfers;

    @ApiModelProperty("描述对象所属的服务器引用")
    private List<LfsRef> refs;

    @ApiModelProperty("用于命名 Git LFS 对象的哈希算法。选修的;如果未指定，则默认为 sha256")
    @JsonProperty("hash_algo")
    private String hashAlgo;

    public static class LfsObjectReq {
        @ApiModelProperty(value ="LFS 对象的 OID")
        private String oid;
        @ApiModelProperty(value ="LFS 对象的整数字节大小。必须至少为零。")
        private long size;

        public String getOid() {
            return oid;
        }

        public void setOid(String oid) {
            this.oid = oid;
        }

        public long getSize() {
            return size;
        }

        public void setSize(long size) {
            this.size = size;
        }
    }

    public static class LfsRef {
        @ApiModelProperty(value ="完全合格的服务器参考规范")
        private String name;

        public String getName() {
            return name;
        }

        public void setName(String name) {
            this.name = name;
        }
    }
}
