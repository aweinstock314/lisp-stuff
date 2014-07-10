#!/bin/bash
JARNAME=$1
jar cfm runnable-${JARNAME} runnable-bundle-manifest.mf ${JARNAME} RecursiveJarUtil.class RunWithRecursiveJar.class RunWithRecursiveJar\$1.class
