<template>
    <nav class="navbar">
        <div class="home-navbar">
            <div class="nav-left">
                <img src="../../public/logo.png" alt="" width="30">
                <a href="/help/index.html">
                    <span class="top-title">{{ title }}</span>
                </a>
                <span style="color:#9CA2A6;">|</span>
                <span class="item-menu">
                    <div v-for="item, index in menuList" :key="index">
                        <!-- :class="{ active:item.meta.indexOf(currentPath) != -1 }" -->
                        <router-link
                            class="text_menu"
                            :class="{ active: type == item.type }"
                            style="color: #3f424a;cursor:pointer;"
                            :to="item.link"
                            @click.native="getType(item.type)"
                        >
                            {{ item.text }}
                        </router-link>
                    </div>
                </span>
            </div>
            <div class="nav-right">
                <!-- <el-input class="input-search" prefix-icon="el-icon-search" placeholder="请输入搜索..."></el-input> -->
                <SearchBox class="input-search" />
                <el-dropdown style="margin-left: 20px;" trigger="click">
                    <span class="el-dropdown-link">
                        登录 / 注册 <i class="el-icon-arrow-down el-icon-caret-bottom"></i>
                    </span>
                    <el-dropdown-menu slot="dropdown">
                        <el-dropdown-item icon="el-icon-user">登 录</el-dropdown-item>
                        <el-dropdown-item icon="el-icon-postcard">注 册</el-dropdown-item>
                    </el-dropdown-menu>
                </el-dropdown>
            </div>
        </div>
    </nav>
</template>

<script>
//   import SearchBox from '@theme/components/SearchBox.vue';
import SearchBox from '@vuepress/plugin-search/SearchBox.vue';
export default {
    name: 'Navbar',
    components: { SearchBox },
    data() {
        return {
            title: 'Fo Library',
            type: 'productGuide'
        };
    },
    computed: {
        menuList() {
            console.log(this.$site,this.$page)
            return this.$site.themeConfig.nav
        },
        currentPath() {
            return this.$route.path.split('.html')[0]
        },
    },
    watch:{
        $route() {
            this.handleInit()
        }
    },
    mounted() {
        this.handleInit()
    },
    methods:{
        getType(type){
            this.type = type
            this.$emit('getType', type)
        },
        handleInit() {
            if (this.currentPath.startsWith('/api')) {
                this.type = 'openApi'
            } else if (this.currentPath.startsWith('/qa')) {
                this.type = 'QA'
            } else {
                this.type = 'productGuide'
            }
            this.getType(this.type)
        }
    }
}
</script>

<style lang="scss">
.navbar {
    position: inherit;
    display: flex;
    justify-content: space-between;
    align-items: center;
    background-color: linear-gradient( 270deg, #EEF7FC 0%, #F7F8FC 65%, #F8FBFE 100%);
    color: #fff;
    height: 72px;
}

.navbar-links {
    display: flex;
    gap: 15px;
    list-style: none;
    padding: 0;
    margin: 0;
}

.home-navbar {
    color: #3F424A;
    display: flex;
    height: 72px;
    width: 100%;
    padding-left: 50px;
    padding-right: 50px;
    justify-content: space-between;
    align-items: center;
    // border-bottom: 1px solid #1890ff;
    font-size: 14px;

    .nav-left {
        display: flex;
        align-items: center;

        .top-title {
            font-weight: 600;
            font-size: 18px;
            line-height: 72px;
            color: #3F424A;
            margin: auto 20px;
        }

        .item-menu {
            display: flex;
            margin: auto 20px;

            .text_menu {
                font-size: 14px;
                margin: auto 15px;
                line-height: 72px;
                display: inline-block;
                height: 72px;
                cursor: pointer;
                box-sizing: border-box;
                transition: all 0.2s;
                background-position: 50% 50px;
                background-repeat: no-repeat;

                &.active {
                    background-image: url('../../public/selected.png');
                    // color: #1890FF !important;
                    font-weight: 600;
                    // border-bottom: 5px solid #1890ff;
                }

                &:hover {
                    // background: #fff;
                    background-image: url('../../public/selected.png');
                    // color: #1890FF !important;
                    font-weight: 600;
                    // border-bottom: 5px solid #1890ff;
                }
            }
        }
    }

    .nav-right {
        display: flex;
        align-items: center;
        .input-search {
            input {
                width: 230px;
                border: 1px solid rgba(151, 151, 151, 0.26);
            }
        }
    }

    .el-dropdown-link {
        cursor: pointer;
        color: #646A74;
    }

    .el-icon-arrow-down {
        font-size: 12px;
    }
}
</style>
