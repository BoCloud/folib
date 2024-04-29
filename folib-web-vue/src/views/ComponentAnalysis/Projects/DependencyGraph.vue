<template>
    <div class="dependency-graph">
        <a-spin :spinning="spinning">
            <div class="by-flex by-p-l-40">
                <div class="by-flex by-m-r-20" v-if="routeComponentId">
                    <a-switch v-model="showCompleteGraph">
                        <a-icon slot="checkedChildren" type="check"/>
                        <a-icon slot="unCheckedChildren" type="close"/>
                    </a-switch>
                    <div class="by-m-l-10">{{ $t('Projects.ShowCompleteGraph') }}</div>
                </div>
                <div class="by-flex">
                    <a-switch v-model="isHighlighted">
                        <a-icon slot="checkedChildren" type="check"/>
                        <a-icon slot="unCheckedChildren" type="close"/>
                    </a-switch>
                    <div class="by-m-l-10">{{ $t('Projects.HighlightComponents') }}</div>
                </div>
            </div>
            <div class="graph-container">
                <vue2-org-tree
                    :data="treeData"
                    :horizontal="true"
                    collapsable
                    :props="treeProps"
                    :label-class-name="labelClassName"
                    @on-expand="onExpand"
                    @on-node-click="handleClickTreeNode"
                    :render-content="renderContent"
                />
            </div>
        </a-spin>
    </div>
</template>

<script>
import {
    getComponentsAndServices,
    getComponentsAndServicesByComponent,
    getAllByComponentId
} from "@/api/projects";

export default {
    name: "DependencyGraph",
    props: {
        uuid: String,
        proName: String
    },
    data() {
        return {
            showCompleteGraph: false,
            isHighlighted: false,
            spinning: false,
            data: {},
            treeProps: {
                label: 'purl',
                children: 'children',
                expand: 'expand'
            },
            routeComponentId: '',
            componentDataByIdKey: {}
        }
    },
    computed: {
        treeData() {
            if (!this.showCompleteGraph && this.routeComponentId) {
                return {
                    ...this.data,
                    children: this.getFilterTreeData(this.data.children)
                }
            } else {
                if (this.routeComponentId) this.getAllTreeData()
                return this.data
            }
        }
    },
    mounted() {
        this.routeComponentId = this.$route.query.componentId
        this.initTreeData()
    },
    methods: {
        initTreeData() {
            getComponentsAndServices(this.uuid).then(res => {
                if (res.status === 200) {
                    res.data.forEach(item => {
                        item.isLoaded = !!this.routeComponentId
                        item.expand = !!this.routeComponentId
                        if (item.directDependencies) {
                            item.children = [
                                ...JSON.parse(item.directDependencies)
                            ]
                        }
                    })
                    this.data = {
                        purl: this.proName,
                        isLoaded: true,
                        expand: !!this.routeComponentId,
                        children: [
                            ...res.data
                        ]
                    }
                    if (this.routeComponentId) this.getAllData()
                }
            })
        },
        getChildData(data) {
            getComponentsAndServicesByComponent(data.uuid).then(res => {
                if (res.status === 200) {
                    res.data.forEach(item => {
                        item.isLoaded = false
                        if (item.directDependencies) {
                            item.children = [
                                ...JSON.parse(item.directDependencies)
                            ]
                        }
                    })
                    data.isLoaded = true
                    data.children = [
                        ...res.data
                    ]
                    this.handleExpand(data)
                }
            })
        },
        getAllData() {
            this.spinning = true
            getAllByComponentId(this.uuid, this.routeComponentId).then(res => {
                if (res.status === 200) {
                    this.componentDataByIdKey = res.data
                    this.getAllTreeData()
                }
            }).finally(()=> {
                this.spinning = false
            })
        },
        getAllTreeData() {
            this.data.children.forEach(item => {
                item.dependencyGraph = this.componentDataByIdKey[item.uuid].dependencyGraph
                this.dealWithData(item)
            })
        },
        dealWithData(data) {
            if (!data.dependencyGraph) return data
            const children = []
            data.dependencyGraph.forEach(id => {
                if (id !== data.uuid) {
                    const item = {
                        ...JSON.parse(JSON.stringify(this.componentDataByIdKey[id])),
                        isLoaded: true,
                        expand: true
                    }
                    children.push(item)
                    this.dealWithData(item)
                }
            })
            data.children = [ ...children ]
        },
        getFilterTreeData(data) {
            if (!data || !data.length) return []
            return data.filter(item => {
                item.children = this.getFilterTreeData(item.children)
                if (item.children && item.children.length) {
                    return true
                } else {
                    return item.uuid === this.routeComponentId
                }
            })
        },
        labelClassName(item) {
            return item.uuid === this.routeComponentId && this.routeComponentId ? 'remark-node' : ''
        },
        renderContent(h, item) {
            return (
                <div class="by-pointer">
                    <span class="item_name">
                        {item.purl}
                    </span>
                    {this.isHighlighted && item.latestVersion &&
                        <a-icon type="warning" theme="filled" style="color: #ffc107"/>}
                </div>
            )
        },
        collapse(list) {
            list.forEach((child) => {
                if (child.expand) child.expand = false
                child.children && this.collapse(child.children);
            });
        },
        onExpand(e, data) {
            if (data.isLoaded) {
                this.handleExpand(data)
            } else {
                e.target.classList.add('node-loading-btn')
                this.getChildData(data)
            }
        },
        handleExpand(data) {
            if ("expand" in data) {
                data.expand = !data.expand;
                if (!data.expand && data.children) {
                    this.collapse(data.children);
                }
            } else {
                this.$set(data, "expand", true);
            }
        },
        handleClickTreeNode(node, item) {
            if (!item.uuid) return
            this.$router.push({
                path: `/componentDetail/${item.uuid}`
            })
        }
    }
}
</script>
<style scoped lang="scss">
.graph-container {
    width: 100%;
    overflow-x: auto;
}

.org-tree-container {
    display: inline-block;
    padding: 15px;
    background-color: #fafafa;
}

::v-deep .org-tree-node-label .org-tree-node-label-inner {
    padding: 1px 2.5px;
    font-size: 12px;
    white-space: nowrap;
}

::v-deep .remark-node {
    border: solid 2.5px rgba(32, 168, 216, 0.99);
}

::v-deep .node-loading-btn {
    cursor: wait;
}
</style>