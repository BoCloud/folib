<template>
    <!-- <aside class="sidebar">
      <ul>
        <li v-for="item in sidebarItems" :key="item.text">
          <router-link class="text-link" :to="item.link">{{ item.text }}</router-link>
        </li>
      </ul>
    </aside> -->
    <aside class="sidebar">
        <el-menu  :default-active="active" class="doc-menu" :key="key" :collapse="isCollapse">
            <router-link class="left_to_home" to="/">
                <span class="el-icon-back"></span> 帮助中心主页
            </router-link>
            <el-submenu :index="item.title" v-for="item,index in menuList" :key="index">
                <template slot="title">
                    <!-- <i class="el-icon-location"></i> -->
                    <span slot="title">{{ item.title }}</span>
                </template>
                <template v-for="itm,i in item.children" >
                    <el-submenu :index="itm.title" v-if="itm.children" :key="i + 900">
                        <span slot="title">{{ itm.title }}</span>
                        <el-menu-item v-for="im in itm.children" :index="im.link" :key="im.link">
                            <router-link :title="im.title" class="text-link" :to="im.link">{{ im.title }}</router-link>
                        </el-menu-item>
                    </el-submenu>

                    <el-menu-item v-else :index="itm.link" >
                        <router-link :title="itm.title" class="text-link" :to="itm.link">{{ itm.title }}</router-link>
                    </el-menu-item>
                </template>
            </el-submenu>
        </el-menu>
    </aside>
</template>

<script>
export default {
    name: 'Sidebar',
    props: {
        type: {
            type: String,
            required: 'productGuide',
        },
    },
    data() {
        return {
            isCollapse:false,
            active:'',
            key:0
        };
    },
    computed:{
        menuList(){
            const list = this.$site.themeConfig.sidebar[this.type]
            return list
        },
    },
    watch:{
        $route(val){
            const active = val.path.split('.html')[0]
            this.getActive(active)
        },
        type(){
            this.key ++
        }
    },
    mounted() {
        this.active = this.$page.path.split('.html')[0]
    },
    methods: {
        getActive(path){
            this.$nextTick(() => {
                this.active = path
            })
        }
    }
};
</script>

<style lang="scss">
.sidebar {
    position: inherit;
    height: calc(100vh - 72px);
    overflow: auto;
    width: 288px;
    background: linear-gradient( 180deg, #F8FBFE 0%, #FFFFFF 100%);
    border-right: none !important; 
}

.doc-menu{
    position: relative;
    background: transparent;
    &.el-menu{
        border-right: none;
        padding: 20px;
        padding-top: 70px;

        & > .el-submenu{
            > .el-submenu__title{
                .el-submenu__icon-arrow{
                    right: 40px;
                    font-size: 15px;

                    &::before{
                        content:'\e78f'
                    }
                }
            }
        }

        .el-menu--inline{
            background: transparent;
        }

        .el-submenu{
            margin-bottom: 15px;

            &:has(.el-submenu){

                .el-submenu__title{
                    font-size: 15px;
                }

                &.is-opened > .el-submenu__title{
                    position: relative;
                    margin-bottom: 20px;
                    font-size: 16px;
                    margin-left: 0px;

                    &:after{
                        position: absolute;
                        display: inline-block;
                        content: "";
                        height: 1px;
                        width: calc(100% - 30px);
                        border-bottom: 1px solid #E4E6E8;
                        left: 20px;
                        top: 50px;
                    }
                }

                & > .el-submenu__title{
                    font-size: 16px;
                    &:after{
                        display: none;
                    }
                }

                .el-submenu{
                    margin-left: 15px;

                    .el-menu-item{
                        margin-left: 25px !important;
                    }
                }
            }
            .el-submenu__title{
                border-radius: 5px;
                color: #333333;
                font-size: 16px;
                font-weight: 600;
                line-height: 36px;
                height: 40px;
                margin-bottom: 5px;
            }

            .el-menu-item{
                border-radius: 5px;
                color: #666666;
                font-size: 15px;
                margin-bottom: 10px;
                width: 90%;
                padding: 0 !important;
                height: 32px;
                line-height: 28px;
                margin-left: 18px;
                
                a{  
                    overflow: hidden;
                    text-overflow: ellipsis;
                    white-space: nowrap;
                    position: relative;
                    padding-left: 20px;
                    padding-right: 10px;
                    display: inline-block;
                    width: 75%;
                    margin-left: 18px;
                    color: #666666;
                    line-height: 32px;

                    &::before{
                      position: absolute;
                      display: none;
                      content:'';
                      height: 4.5px;
                      width: 4.5px;
                      border-radius: 50%;
                      background-color: #1890ff;
                      // border-left: 4.5px solid #1890ff;
                      // border-top: 4.5px solid transparent;
                      // border-right: 4.5px solid transparent;
                      // border-bottom: 4.5px solid transparent;
                      left: 0px;
                      top: 15px;
                    }
                }

                &:hover{
                    background: #DFEDFE;
                    a{
                        color:#007AFF;

                        &::before{
                            display: inline-block;
                        }
                    }
                }

                &.is-active{
                    background: #DFEDFE;

                    a{  
                        color:#007AFF;

                        &::before{
                            display: inline-block;
                        }
                    }
                }
            }
        }
    }

    .left_to_home{
        position: absolute;
        top: 25px;
        margin-left: 20px;
        font-size: 14px;
        color: #0A6AFF;
    }
}
</style>
