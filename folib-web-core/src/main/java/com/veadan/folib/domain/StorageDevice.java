package com.veadan.folib.domain;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Objects;

/**
 * @author veadan
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class StorageDevice {

    /**
     * 存储设备名称
     */
    private String name;

    /**
     * 存储设备类型
     */
    private String type;

    /**
     * 获取总空间
     */
    private long totalSpace;

    /**
     * 获取可用空间
     */
    private long usableSpace;

    /**
     * 获取已用空间
     */
    private long usedSpace;

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        StorageDevice that = (StorageDevice) o;
        return name.equals(that.name);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name);
    }
}
