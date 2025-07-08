package com.folib.domain;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;
import java.util.Objects;

/**
 * @author veadan
 * @date 2024/4/24
 **/
@Builder
@Data
@AllArgsConstructor
@NoArgsConstructor
public class Tree {

    /**
     * label
     */
    private String label;

    /**
     * value
     */
    private String value;

    /**
     * children
     */
    private List<Tree> children;

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        Tree tree = (Tree) o;
        return label.equals(tree.label) &&
                value.equals(tree.value);
    }

    @Override
    public int hashCode() {
        return Objects.hash(label, value);
    }
}
